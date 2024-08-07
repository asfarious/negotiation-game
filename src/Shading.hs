{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Shading where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW

import                         Constants
import                         Projection                          (viewBoard)

boardShader :: Window os RGBAFloat ds
          -> Buffer os (Uniform (B3 Float))
          -> Buffer os (Uniform (B4 Float))
          -> Texture2DArray os (Format RGBAFloat)
          -> Shader os (PrimitiveArray Triangles (B4 Float, B2 Float), PrimitiveArray Triangles (B3 Float, B4 Float, B4 Float)) ()
boardShader win positionBuffer selectedProvBuffer mapTextureArray = do

    --Stream initialization --
    primitiveStream <- toPrimitiveStream fst
    primitivePoints <- toPrimitiveStream snd
    (V3 x z s)      <- getUniform $ const (positionBuffer, 0)
    selectedColor   <- getUniform $ const (selectedProvBuffer, 0)
    
    --Rasterizing the map --
    let primitiveStream2 = fmap (\(point, texPos) -> (viewBoard (V3 x 0 z) s point, texPos)) primitiveStream
    fragmentStream <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitiveStream2
      
    -- A map teture sampler --
    let filter = SamplerFilter Nearest Nearest Nearest Nothing
        edge = (pure Repeat, undefined)
    sampler <- newSampler2DArray $ const (mapTextureArray, filter, edge)
    
    -- Texturing the map --
    let sampleTexture = sample2DArray sampler SampleAuto Nothing
        fragmentStreamTextured  = fmap (sampleTexture . (\(V2 u v) -> V3 u v 0)) fragmentStream
        fragmentStreamProvinces = fmap (sampleTexture . (\(V2 u v) -> V3 u v 1)) fragmentStream
        fragmentStreamHighlight = fmap 
            (\color -> ifThenElse' (color ==* selectedColor) (V4 1 1 1 0.3) (V4 0 0 0 0)) fragmentStreamProvinces
        
    -- Debug points --
    let primitivePoints2 = fmap (\(offset, vertex, color) -> (viewBoard (V3 x 0 z) s (vector offset + vertex), color)) primitivePoints
    fragmentStreamPoints <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitivePoints2
        
    -- Actually drawing --
    let blending = BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero) (V4 0 0 0 0)
    drawWindowColor (const (win, ContextColorOption NoBlending (V4 True True True True))) fragmentStreamTextured
    drawWindowColor (const (win, ContextColorOption blending (V4 True True True True))) fragmentStreamHighlight
    drawWindowColor (const (win, ContextColorOption NoBlending (V4 True True True True))) fragmentStreamPoints

textShader :: Window os RGBAFloat ds
           -> Texture2D os (Format RFloat)
           -- -> V3 Float -- TODO: add the text color as a parameter of some sort
           -> Shader os (PrimitiveArray Triangles (B3 Float, B4 Float, B2 Float, B4 Float)) ()
textShader win atlasTex = do -- (V3 rColor gColor bColor) = do
    primitiveStream <- toPrimitiveStream id
    let primitiveStream2 = fmap (\(pos, vertex@(V4 vertX vertY vertZ vertW), V2 width height, V4 offsetX offsetY texWidth texHeight) 
                            -> (vector pos + V4 (vertX*width) (negate vertY*height) vertZ vertW, V2 (offsetX + vertX*texWidth) (offsetY + vertY*texHeight)))
                                primitiveStream
    fragmentStream <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitiveStream2
    
    let filter = SamplerFilter Linear Linear Linear Nothing
        edge = (pure ClampToBorder, 1)
    sampler <- newSampler2D $ const (atlasTex, filter, edge)
    
    let sampleAtlas = sample2D sampler SampleAuto Nothing Nothing
        bitmapToColor = \alpha -> V4 0 0 0 alpha
        fragmentStreamTextured = fmap (bitmapToColor . sampleAtlas) fragmentStream
    
    let blending = BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero) (V4 0 0 0 0)
    drawWindowColor (const (win, ContextColorOption blending (V4 True True True True))) fragmentStreamTextured


boxShader :: Window os RGBAFloat ds
          -> Shader os (PrimitiveArray Triangles (B4 Float, B4 Float, B4 Float)) ()
boxShader win = do
    primitiveStream <- toPrimitiveStream id
    let primitiveStream2 
            = fmap (\(V4 vertX vertY vertZ vertW, V4 x y width height, color) -> (V4 (x + vertX*width) (y + (negate vertY * height)) vertZ vertW, color)) primitiveStream
    
    fragmentStream <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitiveStream2
    
    let blending = BlendRgbAlpha (FuncAdd, FuncAdd) (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero) (V4 0 0 0 0)
    drawWindowColor (const (win, ContextColorOption NoBlending (V4 True True True True))) fragmentStream