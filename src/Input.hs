{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Input ( Input(..)
             , InputTVars(..)
             , initInput
             , collectInput
             , keyboardCallback
             , mouseScrollCallback
             ) where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (filterM)
import                         Control.Monad.IO.Class               (MonadIO(..), liftIO)
import                         Control.Monad.Extra                  (partitionM)
import                         Control.Concurrent.STM
import qualified               Data.Map.Strict as M
import                         Data.Map.Strict                      (Map)

import                         Constants
import                         Projection                           (relativePos)

collectInput :: (MonadIO m) => Window os c ds -> InputTVars -> (V2 Int -> Bool) -> ContextT GLFW.Handle os m Input
collectInput win inputTVars cursorCondition = do
    maybeCur <- GLFW.getCursorPos win
    let cursorInput = case maybeCur of
                            Just (x, y) -> V2 x y
                            Nothing     -> V2 0 0
        resolvedCursor = resolveCursor cursorInput cursorCondition
    
    (pressedKeys, heldKeys, _) <- pollWith isKeyPressed win (keyButtonsTVar inputTVars) allKeys   
    (mouseInput, _, _)  <- pollWith isMBPressed win (mouseButtonsTVar inputTVars) allPossible
           
    toScroll <- liftIO $ atomically $ do
        scrolled <- readTVar $ scrollTVar inputTVars
        writeTVar (scrollTVar inputTVars) (0 :: Double)
        pure scrolled
        
    pure $ MkInput { keyboardInput = Right (pressedKeys, heldKeys)
                   , mouseButtonInput = mouseInput
                   , scrollInput = toScroll
                   , cursorPosition = resolvedCursor
                   }

justPressed :: (Ord a) => [a] -> [a] -> TVar (Map a Int) -> STM [a]
justPressed pressed released tvar = do
        buttonState <- readTVar $ tvar
        let buttonState'  = foldr (\x state -> M.adjust (+1) x state) buttonState pressed
            buttonState'' = foldr (\x state -> M.adjust (const 0) x state) buttonState' released
            justPressedButtons = fmap fst . filter (\(_, n) -> n == 1) . M.toList $ buttonState''
        writeTVar tvar buttonState''
        pure justPressedButtons

resolveCursor :: (V2 Double) -> (V2 Int -> Bool) -> Either (V2 Int) (V2 Float)        
resolveCursor cursor@(V2 x y) condition = let roundedCursor = fmap round cursor 
                                 in if condition roundedCursor 
                                    then Left roundedCursor
                                    else Right . fmap realToFrac $ (\(V3 x y z) -> V2 x y) $ relativePos x y

pollWith :: (MonadIO m, Ord a) 
         => (Window os c ds -> a -> ContextT GLFW.Handle os m Bool) 
         -> Window os c ds 
         -> TVar (Map a Int)
         -> [a] 
         -> ContextT GLFW.Handle os m ([a], [a], [a])
pollWith isOk win tvar polled = do
                            (pressed, released) <- partitionM (isOk win) polled
                            justPressed <- liftIO $ atomically $ justPressed pressed released tvar
                            pure (justPressed, pressed, released)

allPossible :: (Enum a, Bounded a) => [a]
allPossible = enumFrom minBound

allKeys :: [GLFW.Key] -- GLFW.Key'Unknown is not an actual key
allKeys = drop 1 allPossible

isKeyPressed :: (MonadIO m) => Window os c ds -> GLFW.Key -> ContextT GLFW.Handle os m Bool
isKeyPressed win key = GLFW.getKey win key >>= \status -> case status of
        Just GLFW.KeyState'Pressed -> pure True
        _                          -> pure False

isMBPressed :: (MonadIO m) => Window os c ds -> GLFW.MouseButton -> ContextT GLFW.Handle os m Bool
isMBPressed win button = GLFW.getMouseButton win button >>= \status -> case status of
        Just GLFW.MouseButtonState'Pressed -> pure True
        _                                  -> pure False

keyboardCallback :: TVar String -> TVar Bool -> Char -> IO ()
keyboardCallback textTVar isTextTVar = 
    \c -> atomically $ do
        isText <- readTVar isTextTVar
        case isText of
            False -> pure ()
            True  -> modifyTVar' textTVar (++[c]) -- TODO: Replace String with Text

mouseScrollCallback :: TVar Double -> Double -> Double -> IO ()
mouseScrollCallback scrolledTVar = \_xOffset yOffset -> atomically $ modifyTVar' scrolledTVar (+yOffset)

initInput :: (MonadIO m) => Window os c ds -> ContextT GLFW.Handle os m InputTVars
initInput win = do
                inputTVars <- liftIO $ atomically $ initInputTVars
                GLFW.setScrollCallback win $ Just $ mouseScrollCallback (scrollTVar inputTVars)
                GLFW.setCharCallback win $ Just $ keyboardCallback (strTVar inputTVars) (isStrTVar inputTVars)
                pure inputTVars

initInputTVars :: STM InputTVars
initInputTVars = do 
                    isStrTVar'        :: TVar Bool                       <- newTVar (False :: Bool)
                    strTVar'          :: TVar String                     <- newTVar ([] :: String)
                    scrollTVar'       :: TVar Double                     <- newTVar (0 :: Double)
                    mouseButtonsTVar' :: TVar (Map GLFW.MouseButton Int) <- newTVar $ M.fromList . zip allPossible $ repeat 0
                    keyButtonsTVar'    :: TVar (Map GLFW.Key Int)         <- newTVar $ M.fromList . zip allKeys $ repeat 0
                    
                    pure MkInputTVars { scrollTVar       = scrollTVar'
                                      , strTVar          = strTVar'
                                      , isStrTVar        = isStrTVar'
                                      , mouseButtonsTVar = mouseButtonsTVar'
                                      , keyButtonsTVar   = keyButtonsTVar'
                                      }
        
data InputTVars = MkInputTVars { scrollTVar       :: TVar Double
                               , strTVar          :: TVar String
                               , isStrTVar        :: TVar Bool
                               , mouseButtonsTVar :: TVar (Map GLFW.MouseButton Int)
                               , keyButtonsTVar   :: TVar (Map GLFW.Key Int)
                               }
                               
data Input = MkInput { keyboardInput    :: Either String ([GLFW.Key], [GLFW.Key])
                     , mouseButtonInput :: [GLFW.MouseButton]
                     , scrollInput      :: Double
                     , cursorPosition   :: Either (V2 Int) (V2 Float)
                     }