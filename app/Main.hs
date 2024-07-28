{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Main where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (unless)
import                         Control.Monad.IO.Class               (MonadIO(..), liftIO)
import                         Control.Concurrent.STM
import qualified               Data.Sequence               as Seq   (empty)
import                         Data.Sequence                        (Seq(..))
--
import                         Constants
import                         Texture
import                         Shading
import                         Projection                           (projectToBoard)
import                         Input
import                         States
import                         Events

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGBA8) 
        (GLFW.WindowConfig displayWidth displayHeight "The Negotiation Game" Nothing [GLFW.WindowHint'Resizable False] Nothing)
    
    mapTexture <- importTexture mapPath mapHeight mapWidth
    (vertexBuffer, quadBuffer, rectBuffer, positionBuffer) <- initializeBuffers
    let initialPosition = V3 (mapQuadWidth/2) (mapQuadHeight/2) initialZoom
    writeBuffer positionBuffer 0 [initialPosition]
    
    isStrTVar'   :: TVar Bool       <- liftIO $ atomically $ newTVar (False :: Bool)
    strTVar'    :: TVar String      <- liftIO $ atomically $ newTVar ([] :: String)
    scrollTVar' :: TVar Double      <- liftIO $ atomically $ newTVar (0 :: Double)
    let inputTVars = MkInputTVars { scrollTVar = scrollTVar'
                                  , strTVar    = strTVar'
                                  , isStrTVar  = isStrTVar'
                                  }
    GLFW.setScrollCallback win $ Just $ mouseScrollCallback scrollTVar'
    GLFW.setCharCallback win $ Just $ keyboardCallback strTVar' isStrTVar'
    
    shader <- compileShader $ boardShader win positionBuffer mapTexture
    
    let mapState = MkMapState { position = initialPosition
                              , cursor   = Just (V4 0 0 0 1)
                              , mapMode  = RawMapMode
                              }
      
    gameLoop vertexBuffer positionBuffer quadBuffer rectBuffer shader win mapState inputTVars

initializeBuffers :: (ContextHandler ctx, MonadIO m) => ContextT ctx os m 
                 (Buffer os (B4 Float, B2 Float), 
                  Buffer os (B4 Float), 
                  Buffer os (B3 Float, B4 Float), 
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
                             
    rectBuffer :: Buffer os (B3 Float, B4 Float) <- newBuffer 800 -- position + color
    writeBuffer rectBuffer 0 $ repeat (V3 0 0 0 , V4 0 0 0 0)
    
    positionBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1 -- Looking-at 2D-position + Zoom level
    writeBuffer positionBuffer 0 [0]
    
    pure (vertexBuffer, quadBuffer, rectBuffer, positionBuffer) 


gameLoop vertexBuffer positionBuffer quadBuffer pointBuffer shader win mapState inputTVars = do
    -- Collect input --
    input <- collectInput win inputTVars

    -- Process events arising from input --
    let (mapState', debugIO) = processEvents mapState input
    
    -- Move the data to the GPU buffers --
    let cursorPointer = case cursor mapState' of
                            Just cur -> normalizePoint cur
                            Nothing  -> V3 0 0 0
    _ <- liftIO $ debugIO
    
    writeBuffer positionBuffer 0 [position mapState']
    let unpackPosition (V3 x z _) = V3 x 0 z
    writeBuffer pointBuffer 0 [(unpackPosition . position $ mapState', (V4 1 0 0 1 :: V4 Float)), (cursorPointer, V4 0 1 0 1)]
    
    -- Render the state --
    render $ do
        clearWindowColor win (V4 1 1 1 1)
        vertexArray <- newVertexArray vertexBuffer
        pointArray <- takeVertices 2 <$> newVertexArray pointBuffer
        quadArray <- newVertexArray quadBuffer
        let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
        let pointPrimitives = toPrimitiveArrayInstanced TriangleStrip (\vertex (offset, color) -> (offset, vertex, color)) quadArray pointArray
        shader (primitiveArray, pointPrimitives)
  
    swapWindowBuffers win
    
    -- Do another iteration --
    closeRequested <- GLFW.windowShouldClose win
    unless (closeRequested == Just True) $
        gameLoop vertexBuffer positionBuffer quadBuffer pointBuffer shader win mapState' inputTVars