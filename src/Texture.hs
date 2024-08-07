{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Texture ( importTexture
               , importTextureFromList
               , importTextureArray
               , fromImage
               , getImage
               , getColorAt
               ) where

import                         Graphics.GPipe                                            
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW       as GLFW
import qualified               Codec.Picture as JP
import                         Data.Word                                 (Word8)
import                         Control.Monad.IO.Class                    (MonadIO, liftIO)


importTextureArray :: (MonadIO m, ContextHandler ctx) 
     => [FilePath]
     -> Int 
     -> Int
     -> ContextT ctx os m (Texture2DArray os (Format RGBAFloat))
importTextureArray filePaths height width = do
        let layers = length filePaths
            size = V3 height width layers
        rawTextures <- liftIO $ fmap concat . traverse getTextureRaw $ filePaths
        textureArray <- newTexture2DArray RGBA8 size 1
        writeTexture2DArray textureArray 0 0 size rawTextures
        pure textureArray

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

importTextureFromList :: (MonadIO m, ContextHandler ctx) 
     => [(V4 Word8)]
     -> Int 
     -> Int 
     -> ContextT ctx os m (Texture2D os (Format RGBAFloat))
importTextureFromList rawTexture height width = do
        let size = V2 height width
        texture <- newTexture2D RGBA8 size 1
        writeTexture2D texture 0 0 size rawTexture
        pure texture


getTextureRaw :: FilePath -> IO [(V4 Word8)]
getTextureRaw = fmap fromImage . getImage
                      
fromImage :: JP.Image JP.PixelRGBA8 -> [V4 Word8]
fromImage = fmap pixelToVector . extractPixels
    where pixelToVector (JP.PixelRGBA8 r g b a) = V4 r g b a -- r, g, b, a are just Word8ß values
          extractPixels image = [ JP.pixelAt image x y | y <- [0..(JP.imageHeight image - 1)], x <- [0..(JP.imageWidth image - 1)] ] 
          -- EXTREMELY SUGARY

getImage :: FilePath -> IO (JP.Image JP.PixelRGBA8)
getImage filePath = JP.readPng filePath >>= \x -> case x of
        Prelude.Left  _            -> error "YOU FORGOT YOUR SPRITES MATE"
        Prelude.Right dynamicImage -> pure . JP.convertRGBA8 $ dynamicImage
        
getColorAt :: JP.Image JP.PixelRGBA8 -> Int -> Int -> V4 Float
getColorAt image x y = case JP.pixelAt image x y of
        JP.PixelRGBA8 r g b a -> V4 (convert r) (convert g) (convert b) (convert a)
    where convert c = fromIntegral c / 255