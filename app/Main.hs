{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Main where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (unless)
import                         Control.Monad.IO.Class               (MonadIO(..), liftIO)
import                         Control.Concurrent.STM
--
import                         Constants
import                         Texture
import                         Shading
import                         Projection                           (projectToBoard)
import                         Input

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB16) 
        (GLFW.WindowConfig displayWidth displayHeight "The Negotiation Game" Nothing [GLFW.WindowHint'Resizable False] Nothing)
    
    mapTexture <- importTexture mapPath mapHeight mapWidth
    (vertexBuffer, quadBuffer, rectBuffer, positionBuffer) <- initializeBuffers
    let initialPosition = V3 (mapQuadWidth/2) (mapQuadHeight/2) initialZoom
    writeBuffer positionBuffer 0 [initialPosition]
    
    debugTVar :: TVar Bool <- liftIO $ atomically $ newTVar (False :: Bool)
    scrolledTVar :: TVar Double <- liftIO $ atomically $ newTVar (0 :: Double)
    GLFW.setScrollCallback win $ Just $ mouseScrollCallback scrolledTVar
    
    shader <- compileShader $ boardShader win positionBuffer mapTexture
      
    gameLoop vertexBuffer positionBuffer quadBuffer rectBuffer shader win initialPosition scrolledTVar debugTVar

initializeBuffers :: (ContextHandler ctx, MonadIO m) => ContextT ctx os m 
                 (Buffer os (B4 Float, B2 Float), 
                  Buffer os (B4 Float), 
                  Buffer os (B3 Float, B3 Float), 
                  Buffer os (Uniform (B3 Float)))
initializeBuffers = do
    vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [ (V4 0            0 0             1, V2 0 0)
                               , (V4 mapQuadWidth 0 0             1, V2 1 0)
                               , (V4 0            0 mapQuadHeight 1, V2 0 1)
                               , (V4 mapQuadWidth 0 mapQuadHeight 1, V2 1 1)
                               ]
                               
    quadBuffer :: Buffer os (B4 Float) <- newBuffer 4       -- a quad that will then be rotated, scaled, translated,
    writeBuffer quadBuffer 0 ([ V4   0.1  0 (-0.1) 1        -- and colored for debug purposes
                             ,  V4   0.1  0  0.1   1
                             ,  V4 (-0.1) 0 (-0.1) 1
                             ,  V4 (-0.1) 0  0.1   1
                             ] :: [V4 Float])
                             
    rectBuffer :: Buffer os (B3 Float, B3 Float) <- newBuffer 800 -- position + color
    writeBuffer rectBuffer 0 $ repeat (V3 0 0 0, V3 0 0 0)
    
    positionBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1 -- Looking-at 2D-position + Zoom level
    writeBuffer positionBuffer 0 [0]
    
    pure (vertexBuffer, quadBuffer, rectBuffer, positionBuffer) 


gameLoop vertexBuffer positionBuffer quadBuffer pointBuffer shader win position@(V3 _ _ s) scrolledTVar debugTVar = do
    maybeCur <- GLFW.getCursorPos win
    let cursor = case maybeCur of
                            Just (x, y) -> V4 (realToFrac x * 2 / fromIntegral displayWidth - 1) (1 - realToFrac y * 2 / fromIntegral displayHeight) (-1) 1
                            Nothing     -> V4 0 0 (-1) 1
    
    writeBuffer positionBuffer 0 [position]
    let unpackPosition (V3 x z _) = V3 x 0 z
    let projectedCursor = normalizePoint $ projectToBoard (unpackPosition position) s cursor
    writeBuffer pointBuffer 0 [(unpackPosition position, V3 1 0 0), (projectedCursor, V3 0 1 0)]
  
    render $ do
        clearWindowColor win (V3 1 1 1)
        vertexArray <- newVertexArray vertexBuffer
        pointArray <- takeVertices 2 <$> newVertexArray pointBuffer
        quadArray <- newVertexArray quadBuffer
        let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
        let pointPrimitives = toPrimitiveArrayInstanced TriangleStrip (\vertex (offset, color) -> (offset, vertex, color)) quadArray pointArray
        shader (primitiveArray, pointPrimitives)
  
    swapWindowBuffers win

    input <- getInput win debugTVar
    toScrollKeyed <- scrollKey win
    toScroll <- liftIO $ atomically $ do
        scrolled <- readTVar scrolledTVar
        writeTVar scrolledTVar (0 :: Double)
        pure scrolled
  
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
        gameLoop vertexBuffer positionBuffer quadBuffer pointBuffer shader win position' scrolledTVar debugTVar
        

adjustZoom :: V3 Float -> Float -> V3 Float
adjustZoom (V3 x y s) zoom = V3 x y s'
    where s' = max (s + zoom) zoomMinBound