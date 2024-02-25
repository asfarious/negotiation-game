{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Main where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (unless)
import                         Control.Monad.IO.Class               (liftIO)
import                         Control.Concurrent.STM
--
import                         Constants

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.WindowConfig displayWidth displayHeight "The Negotiation Game" Nothing [GLFW.WindowHint'Resizable False] Nothing)

    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [ (V4 0 0 0 1, V3 0 1 0)
                               , (V4 10 0 0 1, V3 1 0 0)
                               , (V4 0 10 0 1, V3 1 0 0)
                               , (V4 10 10 0 1, V3 1 0 0)
                               ]
                               
    positionBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1 -- Looking-at 2D-position + Zoom level
    writeBuffer positionBuffer 0 [V3 0 0 0.5]
    
    debugTVar :: TVar Bool <- liftIO $ atomically $ newTVar (False :: Bool)
    
    scrolledTVar :: TVar Double <- liftIO $ atomically $ newTVar (0 :: Double)
    GLFW.setScrollCallback win $ Just $ mouseScrollCallback scrolledTVar
    
    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      (V3 x y s) <- getUniform $ const (positionBuffer, 0)
      let primitiveStream2 = fmap (\(point, color) -> (viewBoard (V3 x y 0) s point, color) ) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitiveStream2
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer positionBuffer shader win (V3 0 0 0.5) scrolledTVar debugTVar

loop vertexBuffer positionBuffer shader win position scrolledTVar debugTVar = do
  writeBuffer positionBuffer 0 [position]
  render $ do
    clearWindowColor win (V3 1 1 1)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
    shader primitiveArray
  swapWindowBuffers win

  input <- getInput win debugTVar
  toScroll <- liftIO $ atomically $ do
    scrolled <- readTVar scrolledTVar
    writeTVar scrolledTVar (0 :: Double)
    pure scrolled
  let zoom :: Float = realToFrac $ toScroll * (negate zoomSpeed)
  let position' = adjustZoom (position + input) zoom    
  
  toDebug <- liftIO $ atomically $ do
    toDebug' <- readTVar debugTVar
    writeTVar debugTVar (False :: Bool)
    pure toDebug'
  if toDebug 
    then liftIO $ putStrLn $ show position
    else pure ()
  
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer positionBuffer shader win position' scrolledTVar debugTVar
    
viewBoard :: Floating a => V3 a -> a -> V4 a -> V4 a
viewBoard at zoom = ((projMat !*! lookMat) !*)
    where 
        projMat = perspective (pi/3) 1 1 100
        lookMat = lookAt (at + zoom *^ (V3 0 (-2) 2)) at (V3 0 1 0)

adjustZoom :: V3 Float -> Float -> V3 Float
adjustZoom (V3 x y s) zoom = V3 x y s'
    where s' = max (s + zoom) zoomMinBound

mouseScrollCallback :: TVar Double -> Double -> Double -> IO ()
mouseScrollCallback scrolledTVar xOffset yOffset = atomically $ modifyTVar' scrolledTVar (+yOffset)
        
getInput :: Window os c ds -> TVar Bool -> ContextT GLFW.Handle os IO (V3 Float)
getInput win debugTVar = do
        let keys = [GLFW.Key'W, GLFW.Key'A, GLFW.Key'D, GLFW.Key'S, GLFW.Key'M]
        arePressed <- traverse (isPressed win) keys
        case arePressed of
            [True, _,    _,    _,       _] -> pure $ V3  0                    cameraSpeed         0
            [_,    True, _,    _,       _] -> pure $ V3 (negate cameraSpeed)  0                   0
            [_,    _,    True, _,       _] -> pure $ V3  cameraSpeed          0                   0
            [_,    _,    _,    True,    _] -> pure $ V3  0                   (negate cameraSpeed) 0
            [_,    _,    _,    _,    True] -> do 
                                                liftIO $ atomically $ writeTVar debugTVar True
                                                pure $ V3 0 0 0
            [_,    _,    _,    _,       _] -> pure $ V3 0 0 0

isPressed :: Window os c ds -> GLFW.Key -> ContextT GLFW.Handle os IO Bool
isPressed win key = GLFW.getKey win key >>= \status -> case status of
        Just GLFW.KeyState'Pressed -> pure True
        _                          -> pure False