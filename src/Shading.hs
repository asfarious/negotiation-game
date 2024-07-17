{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Shading where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW

import                         Constants
import                         Projection                          (viewBoard)

boardShader :: Window os RGBFloat ds
          -> Buffer os (Uniform (B3 Float))
          -> Texture2D os (Format RGBFloat)
          -> Shader os (PrimitiveArray Triangles (B4 Float, B2 Float), PrimitiveArray Triangles (B3 Float, B4 Float, B3 Float)) ()
boardShader win positionBuffer mapTexture = do

    --Stream initialization --
    primitiveStream <- toPrimitiveStream fst
    primitivePoints <- toPrimitiveStream snd
    (V3 x z s) <- getUniform $ const (positionBuffer, 0)
    
    --Rasterizing the map --
    let primitiveStream2 = fmap (\(point, texPos) -> (viewBoard (V3 x 0 z) s point, texPos)) primitiveStream
    fragmentStream <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitiveStream2
      
    -- A map teture sampler --
    let filter = SamplerFilter Nearest Nearest Nearest Nothing
        edge = (pure Repeat, undefined)
    sampler <- newSampler2D $ const (mapTexture, filter, edge)
    
    -- Texturing the map --
    let sampleTexture = sample2D sampler SampleAuto Nothing Nothing
        fragmentStreamTextured = fmap sampleTexture fragmentStream
      
    -- Debug points --
    let primitivePoints2 = fmap (\(offset, vertex, color) -> (viewBoard (V3 x 0 z) s (vector offset + vertex), color)) primitivePoints
    fragmentStreamPoints <- rasterize (const (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (V2 displayWidth displayHeight), DepthRange 0 1)) primitivePoints2
        
    -- Actually drawing --
    drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStreamTextured
    drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStreamPoints