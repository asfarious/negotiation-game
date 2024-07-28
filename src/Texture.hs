{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Texture where

import                         Graphics.GPipe                                            
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW       as GLFW
import qualified               Codec.Picture as JP
import                         Data.Word                                 (Word8)
import                         Control.Monad.IO.Class                    (MonadIO, liftIO)

importTexture :: (MonadIO m, ContextHandler ctx) 
     => FilePath
     -> Int 
     -> Int 
     -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
importTexture filePath height width = do
        let size = V2 height width
        rawTexture <- liftIO $ getTextureRaw filePath
        texture <- newTexture2D RGBA8 size 1
        writeTexture2D texture 0 0 size rawTexture
        pure texture


getTextureRaw :: FilePath -> IO [(V4 Word8)]
getTextureRaw filePath = JP.readPng filePath >>= \x -> case x of
        Prelude.Left  _            -> error "YOU FORGOT YOUR SPRITES MATE"
        Prelude.Right dynamicImage -> pure . fromImage . JP.convertRGBA8 $ dynamicImage
                      
fromImage :: JP.Image JP.PixelRGBA8 -> [V4 Word8]
fromImage = fmap pixelToVector . extractPixels
    where pixelToVector (JP.PixelRGBA8 r g b a) = V4 r g b a -- r, g, b, a are just Word16 values
          extractPixels image = [ JP.pixelAt image x y | y <- [0..(JP.imageHeight image - 1)], x <- [0..(JP.imageWidth image - 1)] ] 
          -- EXTREMELY SUGARY