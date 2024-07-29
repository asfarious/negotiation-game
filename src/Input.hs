{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Input where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (filterM)
import                         Control.Monad.IO.Class               (MonadIO(..), liftIO)
import                         Control.Concurrent.STM
import                         Data.Bool                            (bool)
import                         Data.Sequence

import                         Constants

collectInput :: (MonadIO m) => Window os c ds -> InputTVars -> ContextT GLFW.Handle os m Input
collectInput win inputTVars = do
    maybeCur <- GLFW.getCursorPos win
    let cursorInput = case maybeCur of
                            Just (x, y) -> V2 (realToFrac x * 2 / fromIntegral displayWidth - 1) (1 - realToFrac y * 2 / fromIntegral displayHeight)
                            Nothing     -> V2 0 0
    
    keyInput :: [GLFW.Key] <- pollWith isKeyPressed win allPossible
    mouseInput :: [GLFW.MouseButton] <- pollWith isMBPressed win allPossible
    toScroll <- liftIO $ atomically $ do
        scrolled <- readTVar $ scrollTVar inputTVars
        writeTVar (scrollTVar inputTVars) (0 :: Double)
        pure scrolled
        
    pure $ MkInput { keyboardInput = Right keyInput
                   , mouseButtonInput = mouseInput
                   , scrollInput = toScroll
                   , cursorPosition = cursorInput
                   }

allPossible :: (Enum a, Bounded a) => [a]
allPossible = enumFrom (succ minBound)

pollWith :: (MonadIO m) => (Window os c ds -> a -> ContextT GLFW.Handle os m Bool) -> Window os c ds -> [a] -> ContextT GLFW.Handle os m [a]
pollWith isOk win = filterM (isOk win)

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
mouseScrollCallback scrolledTVar = \xOffset yOffset -> atomically $ modifyTVar' scrolledTVar (+yOffset)
        
data InputTVars = MkInputTVars { scrollTVar :: TVar Double
                               , strTVar    :: TVar String
                               , isStrTVar  :: TVar Bool
                               }
                               
data Input = MkInput { keyboardInput    :: Either String [GLFW.Key]
                     , mouseButtonInput :: [GLFW.MouseButton]
                     , scrollInput      :: Double
                     , cursorPosition   :: V2 Float
                     }