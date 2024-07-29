{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module RenderBoard (initBoardRenderer) where

import                         Graphics.GPipe
import                         Control.Monad.IO.Class               (MonadIO(..))

import                         Constants
import                         Shading
import                         States

initBoardRenderer initialPosition win mapTexture = do
        vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer 4
        writeBuffer vertexBuffer 0 [ (V4 0            0 0             1, V2 0 0)
                                   , (V4 mapQuadWidth 0 0             1, V2 1 0)
                                   , (V4 0            0 mapQuadHeight 1, V2 0 1)
                                   , (V4 mapQuadWidth 0 mapQuadHeight 1, V2 1 1)
                                   ]
                                   
        quadBuffer :: Buffer os (B4 Float) <- newBuffer 4   -- a quad that will then be rotated, scaled, translated,
        writeBuffer quadBuffer 0 ([ V4 (-0.1) 0   0.1   1   -- and colored for debug purposes
                                  , V4   0.1  0   0.1   1
                                  , V4 (-0.1) 0 (-0.1)  1
                                  , V4   0.1  0 (-0.1)  1
                                  ] :: [V4 Float])
                                 
        pointBuffer :: Buffer os (B3 Float, B4 Float) <- newBuffer 800 -- position + color
        writeBuffer pointBuffer 0 $ repeat (V3 0 0 0, V4 0 0 0 0)
        
        positionBuffer :: Buffer os (Uniform (B3 Float)) <- newBuffer 1 -- Looking-at 2D-position + Zoom level
        writeBuffer positionBuffer 0 [initialPosition]
        
        shadeBoard <- compileShader $ boardShader win positionBuffer mapTexture
        
        pure (boardPreRenderer pointBuffer positionBuffer, boardRenderer vertexBuffer quadBuffer pointBuffer shadeBoard)


boardPreRenderer :: (ContextHandler ctx, MonadIO m) 
                 =>  Buffer os (B3 Float, B4 Float)
                 ->  Buffer os (Uniform (B3 Float))
                 ->  MapState 
                 ->  ContextT ctx os m () 
                 
boardPreRenderer pointBuffer positionBuffer mapState = do
        -- Move the data to the GPU buffers --
        let cursorPointer = case cursor mapState of
                                Just cur -> normalizePoint cur
                                Nothing  -> V3 0 0 0
        
        writeBuffer positionBuffer 0 [position mapState]
        let unpackPosition (V3 x z _) = V3 x 0 z
        writeBuffer pointBuffer 0 [(unpackPosition . position $ mapState, (V4 1 0 0 1 :: V4 Float)), (cursorPointer, V4 0 1 0 1)]


boardRenderer :: Buffer os (B4 Float, B2 Float) 
              -> Buffer os (B4 Float)
              -> Buffer os (B3 Float, B4 Float)
              -> ((PrimitiveArray Triangles (B4 Float, B2 Float), PrimitiveArray Triangles (B3 Float, B4 Float, B4 Float)) -> Render os ())
              -> Render os ()
              
boardRenderer vertexBuffer quadBuffer pointBuffer shadeBoard = do
        vertexArray <- newVertexArray vertexBuffer
        pointArray <- takeVertices 2 <$> newVertexArray pointBuffer
        quadArray <- newVertexArray quadBuffer
        let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
        let pointPrimitives = toPrimitiveArrayInstanced TriangleStrip (\vertex (offset, color) -> (offset, vertex, color)) quadArray pointArray
        shadeBoard (primitiveArray, pointPrimitives)