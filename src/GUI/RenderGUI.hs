{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module GUI.RenderGUI where --(initGUIRenderer) where

import                         Graphics.GPipe
import                         Control.Monad.IO.Class               (MonadIO(..))
import                         Text.Printf                          (printf)

import                         Constants
import                         Shading
import                         States
import                         Text
import                         Projection                           (relativePos)
import                         GUI.GUIState

initGUIRenderer win atlas = do
        textQuadBuffer :: Buffer os (B4 Float) <- newBuffer 4
        writeBuffer textQuadBuffer 0 [ V4 0 1 0 1
                                     , V4 1 1 0 1
                                     , V4 0 0 0 1
                                     , V4 1 0 0 1
                                     ]
        
        charBuffer :: Buffer os (B3 Float, B2 Float, B4 Float) <- newBuffer 800 -- 3D-position + sprite size + atlas offset + atlas chunk size
        writeBuffer charBuffer 0 $ repeat (V3 0 0 0, V2 0 0, V4 0 0 0 0)
        
        coloredBoxBuffer :: Buffer os (B4 Float, B4 Float) <- newBuffer 800 -- 2D-position + size + color
        writeBuffer coloredBoxBuffer 0 $ repeat (V4 0 0 0 0, V4 0 0 0 0)
        
        shadeText  <- compileShader $ textShader win atlas
        shadeBoxes <- compileShader $ boxShader win
        
        pure (guiPreRenderer charBuffer coloredBoxBuffer, guiRenderer textQuadBuffer charBuffer coloredBoxBuffer shadeText shadeBoxes)


guiPreRenderer :: (ContextHandler ctx, MonadIO m) 
               =>  Buffer os (B3 Float, B2 Float, B4 Float)
               ->  Buffer os (B4 Float, B4 Float)
               ->  Either (V2 Int) (V2 Float)
               ->  Maybe (V3 Float)
               ->  FontAtlas os
               ->  DisplayChar
               ->  GUIState event
               ->  ContextT ctx os m (Int, Int) 
               
guiPreRenderer charBuffer coloredBoxBuffer cur2D cur3D font defaultChar (MkGUIState (_, guiElements)) = do
        let guiBoxes = concatMap (\(MkGUIElement (_, bBox, _, renderElem)) -> renderElem bBox (cur2D, cur3D)) guiElements
            (coloredBoxes, textBoxes) = splitBoxes guiBoxes
            coloredBoxesScaled = fmap scaleBox coloredBoxes
            textSprites = concatMap (textBoxToSprites font defaultChar) textBoxes
        
        writeBuffer coloredBoxBuffer 0 coloredBoxesScaled
        writeBuffer charBuffer 0 textSprites
        
        let textLength = length textSprites
            boxNumber =  length coloredBoxesScaled
        pure (textLength, boxNumber)
        
        
guiRenderer :: Buffer os (B4 Float)
            -> Buffer os (B3 Float, B2 Float, B4 Float)
            -> Buffer os (B4 Float, B4 Float)
            -> ((PrimitiveArray Triangles (B3 Float, B4 Float, B2 Float, B4 Float)) -> Render os ())
            -> ((PrimitiveArray Triangles (B4 Float, B4 Float, B4 Float)) -> Render os ())
            -> Int
            -> Int
            -> Render os ()
              
guiRenderer textQuadBuffer charBuffer coloredBoxBuffer shadeText shadeBoxes textLength boxNumber = do
        quadArray <- newVertexArray textQuadBuffer
        
        coloredBoxArray <- takeVertices boxNumber <$> newVertexArray coloredBoxBuffer
        let coloredBoxPrimitives = toPrimitiveArrayInstanced TriangleStrip (\vertex (bBox, color) -> (vertex, bBox, color)) quadArray coloredBoxArray
        
        charArray <- takeVertices textLength <$> newVertexArray charBuffer
        let charPrimitives = toPrimitiveArrayInstanced TriangleStrip (\vertex (pos, size, atlasPos) -> (pos, vertex, size, atlasPos)) quadArray charArray
        
        shadeBoxes coloredBoxPrimitives
        shadeText charPrimitives
   
   
splitBoxes :: [GUIBox] -> ([(BoundingBox, V4 Float)], [(BoundingBox, String)])
splitBoxes = foldr go ([], [])
    where go (ColoredBox bBox color) (xs, ys) = ((bBox, color) : xs, ys)
          go (TextBox bBox text)     (xs, ys) = (xs,  (bBox, text) : ys)

scaleBox :: (BoundingBox, V4 Float) -> (V4 Float, V4 Float)
scaleBox (V4 x y width height, color) = let (V3 x' y' _) = relativePos (fromIntegral x) (fromIntegral y) 
                                        in (V4 x'
                                               y'
                                              (2 * fromIntegral width / fromIntegral displayWidth)
                                              (2 * fromIntegral height / fromIntegral displayHeight)
                                           , color
                                           )

textBoxToSprites :: FontAtlas os -> DisplayChar -> (V4 Int, String) -> [(V3 Float, V2 Float, V4 Float)]
textBoxToSprites font defaultChar (V4 x y xScale yScale, str) = let x' = fromIntegral x
                                                                    y' = fromIntegral y 
                                                                in makeTextSprites font 
                                                                                   (relativePos x' y') 
                                                                                   (fromIntegral $ displayWidth * xScale) 
                                                                                   (fromIntegral $ displayHeight * yScale) 
                                                                                   defaultChar
                                                                                   str