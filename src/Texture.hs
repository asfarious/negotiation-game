{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Texture where

import                         Graphics.GPipe                                            
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW       as GLFW
import                         Codec.Picture as JP
import                         Data.Word                                 (Word16)
import                         Control.Monad.IO.Class                    (MonadIO, liftIO)

importTexture :: (MonadIO m, ContextHandler ctx) 
     => FilePath
     -> Int 
     -> Int 
     -> ContextT ctx os m (Texture2D os (Format RGBFloat))
importTexture filePath height width = do
        let size = V2 height width
        rawTexture <- liftIO $ getTextureRaw filePath
        texture <- newTexture2D RGB16 size 1
        writeTexture2D texture 0 0 size rawTexture
        pure texture


getTextureRaw :: FilePath -> IO [(V3 Word16)]
getTextureRaw filePath = JP.readPng filePath >>= \x -> case x of
        Prelude.Left  _            -> error "YOU FORGOT YOUR SPRITES MATE"
        Prelude.Right dynamicImage -> pure . fmap pixelToVector . extractPixels . convertRGB16 $ dynamicImage
                where pixelToVector (JP.PixelRGB16 r g b) = V3 r g b -- r, g, b are just Word16 values
                      extractPixels image = [ JP.pixelAt image x y | y <- [0..(imageHeight image - 1)], x <- [0..(imageWidth image - 1)] ] 
                      -- EXTREMELY SUGARY