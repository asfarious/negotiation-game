{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Main where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (unless)
import                         Control.Monad.IO.Class               (liftIO)
import                         Control.Concurrent.STM
import                         Data.Bool                            (bool)
--
import                         Constants
import                         Texture

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB16) 
        (GLFW.WindowConfig displayWidth displayHeight "The Negotiation Game" Nothing [GLFW.WindowHint'Resizable False] Nothing)
    
    mapTexture <- importTexture mapPath mapHeight mapWidth
    
    vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer 4
    --let mapQuadSize = (negate 10) :: Float
    writeBuffer vertexBuffer 0 [ (V4 0            0 0             1, V2 0 0)
                               , (V4 mapQuadWidth 0 0             1, V2 1 0)
                               , (V4 0            0 mapQuadHeight 1, V2 0 1)
                               , (V4 mapQuadWidth 0 mapQuadHeight 1, V2 1 1)
                               ]
                               
    positionBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1 -- Looking-at 2D-position + Zoom level
    let initialPosition = V3 (mapQuadWidth/2) (mapQuadHeight/2) initialZoom
    writeBuffer positionBuffer 0 [initialPosition]
    
    debugTVar :: TVar Bool <- liftIO $ atomically $ newTVar (False :: Bool)
    
    scrolledTVar :: TVar Double <- liftIO $ atomically $ newTVar (0 :: Double)
    GLFW.setScrollCallback win $ Just $ mouseScrollCallback scrolledTVar
    
    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      (V3 x z s) <- getUniform $ const (positionBuffer, 0)
      
      --let pointAt (V4 x y _ _) = V2 (x * convFactorX) (y * convFactorY)
      let primitiveStream2 = fmap (\(point, texPos) -> (viewBoard (V3 x 0 z) s point, texPos)) primitiveStream
      fragmentStream <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitiveStream2
      
      let filter = SamplerFilter Nearest Nearest Nearest Nothing
          edge = (pure Repeat, undefined)
      sampler <- newSampler2D $ const (mapTexture, filter, edge)
      
      let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
          fragmentStreamTextured = fmap sampleTexture fragmentStream
      
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStreamTextured

    loop vertexBuffer positionBuffer shader win initialPosition scrolledTVar debugTVar

loop vertexBuffer positionBuffer shader win position scrolledTVar debugTVar = do
  writeBuffer positionBuffer 0 [position]
  render $ do
    clearWindowColor win (V3 1 1 1)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
    shader primitiveArray
  swapWindowBuffers win

  input <- getInput win debugTVar
  toScrollKeyed <- scrollKey win
  toScroll <- liftIO $ atomically $ do
    scrolled <- readTVar scrolledTVar
    writeTVar scrolledTVar (0 :: Double)
    pure scrolled
  --liftIO $ putStrLn $ show scrollKeyed ++ " " ++ show toScroll
  let zoom :: Float = realToFrac $ (toScroll * mouseScrollSensitivity + toScrollKeyed * keyScrollSensitivity) * (negate zoomSpeed)
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
        lookMat = lookAt (at + zoom *^ (V3 0 1 1)) at (V3 0 1 0)

adjustZoom :: V3 Float -> Float -> V3 Float
adjustZoom (V3 x y s) zoom = V3 x y s'
    where s' = max (s + zoom) zoomMinBound

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
{--
scrollKey win = do
        isPlus <- isPressed win GLFW.Key'PadAdd
        isMinus <- isPressed win GLFW.Key'PadSubsract
        pure (isPlus - isMinus)
--}

isPressed :: Window os c ds -> GLFW.Key -> ContextT GLFW.Handle os IO Bool
isPressed win key = GLFW.getKey win key >>= \status -> case status of
        Just GLFW.KeyState'Pressed -> pure True
        _                          -> pure False