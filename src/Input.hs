{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Input where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad.IO.Class               (liftIO)
import                         Control.Concurrent.STM
import                         Data.Bool                            (bool)

import                         Constants

mouseScrollCallback :: TVar Double -> Double -> Double -> IO ()
mouseScrollCallback scrolledTVar xOffset yOffset = atomically $ modifyTVar' scrolledTVar (+yOffset)
        
getInput :: Window os c ds -> TVar Bool -> ContextT GLFW.Handle os IO (V3 Float)
getInput win debugTVar = do
        let keys = [GLFW.Key'W, GLFW.Key'A, GLFW.Key'S, GLFW.Key'D, GLFW.Key'M]
        arePressed <- traverse (isPressed win) keys
        case arePressed of
            [True, _,    _,    _,       _] -> pure $ V3  0                    (negate cameraSpeed) 0
            [_,    True, _,    _,       _] -> pure $ V3  (negate cameraSpeed) 0                    0
            [_,    _,    True, _,       _] -> pure $ V3  0                    cameraSpeed          0
            [_,    _,    _,    True,    _] -> pure $ V3  cameraSpeed          0                    0
            [_,    _,    _,    _,    True] -> do 
                                                liftIO $ atomically $ writeTVar debugTVar True
                                                pure $ V3 0 0 0
            [_,    _,    _,    _,       _] -> pure $ V3 0 0 0

scrollKey :: Window os c ds -> ContextT GLFW.Handle os IO Double
scrollKey win = (-) 
                <$> fmap (bool 0 1) (isPressed win GLFW.Key'PadAdd) 
                <*> fmap (bool 0 1) (isPressed win GLFW.Key'PadSubtract)

isPressed :: Window os c ds -> GLFW.Key -> ContextT GLFW.Handle os IO Bool
isPressed win key = GLFW.getKey win key >>= \status -> case status of
        Just GLFW.KeyState'Pressed -> pure True
        _                          -> pure False