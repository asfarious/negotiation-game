{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module RenderGUI (initGUIRenderer) where

import                         Graphics.GPipe
import                         Control.Monad.IO.Class               (MonadIO(..))
import                         Text.Printf                          (printf)

import                         Constants
import                         Shading
import                         States
import                         Text
import                         Projection                           (relativePos)

initGUIRenderer win atlas = do
        textQuadBuffer :: Buffer os (B4 Float) <- newBuffer 4
        writeBuffer textQuadBuffer 0 [ V4 0 1 0 1
                                     , V4 1 1 0 1
                                     , V4 0 0 0 1
                                     , V4 1 0 0 1
                                     ]
        
        charBuffer :: Buffer os (B3 Float, B2 Float, B4 Float) <- newBuffer 800 -- 3D-position + sprite size + atlas offset + atlas chunk size
        writeBuffer charBuffer 0 $ repeat (V3 0 0 0, V2 0 0, V4 0 0 0 0)
        
        shadeText <- compileShader $ textShader win atlas
        
        pure (guiPreRenderer charBuffer, guiRenderer textQuadBuffer charBuffer shadeText)


guiPreRenderer :: (ContextHandler ctx, MonadIO m) 
               =>  Buffer os (B3 Float, B2 Float, B4 Float)
               ->  V2 Float
               ->  V3 Float
               ->  FontAtlas os
               ->  DisplayChar
               ->  ContextT ctx os m Int 
               
guiPreRenderer charBuffer cur2D cur3D font defaultChar = do
        let
            cur2DText = case cur2D of
                    V2 x y -> "2D Cursor: " ++ printf "%.2f, %.2f" x y
            cur3DText = case cur3D of
                    V3 x y z -> "3D Cursor: " ++ printf "%.2f, %.2f, %2f" x y z
            textLength = length cur2DText + length cur3DText
            cur2DTextSprites = makeTextSprites font (relativePos 5 80) (fromIntegral displayWidth) (fromIntegral displayHeight) defaultChar cur2DText
            cur3DTextSprites = makeTextSprites font (relativePos 5 160) (fromIntegral displayWidth) (fromIntegral displayHeight) defaultChar cur3DText
        writeBuffer charBuffer 0 $ cur2DTextSprites ++ cur3DTextSprites
        pure textLength
        
        
guiRenderer :: Buffer os (B4 Float)
            -> Buffer os (B3 Float, B2 Float, B4 Float)
            -> ((PrimitiveArray Triangles (B3 Float, B4 Float, B2 Float, B4 Float)) -> Render os ())
            -> Int
            -> Render os ()
              
guiRenderer textQuadBuffer charBuffer shadeText textLength = do
        textQuadArray <- newVertexArray textQuadBuffer
        charArray <- takeVertices textLength <$> newVertexArray charBuffer
        let charPrimitives = toPrimitiveArrayInstanced TriangleStrip (\vertex (pos, size, atlasPos) -> (pos, vertex, size, atlasPos)) textQuadArray charArray
        shadeText charPrimitives