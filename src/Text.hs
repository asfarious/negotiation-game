{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Text where

import                         Graphics.GPipe                                            
import                         FreeType                         -- as FT -- the FreeType stuff is already prefixed with ft_ and FT_
import                         Data.Word                                 (Word8)
import qualified               Data.HashMap.Strict              as HM
import                         Control.Monad.IO.Class                    (MonadIO(..), liftIO)
import                         Control.Monad                             (foldM)
import                         Foreign                                   (peek, peekArray)

import                         Constants

newtype DisplayChar = MkDisplayChar ( V4 Float -- in relative units: atlas offset, width and height of the sprite
                                    , V3 Float -- in pixels: xBearing, yBearing, advance
                                    )

type CharRegister = HM.HashMap Char DisplayChar
                                    
newtype FontAtlas os = MkFont ( FT_Face
                              , CharRegister
                              , Texture2D os (Format RFloat)
                              , Size2
                              , V3 Int --current offset and current line height
                              )

atlasTexture :: FontAtlas os -> Texture2D os (Format RFloat)
atlasTexture (MkFont (_, _, texture, _, _)) = texture

atlasSize :: FontAtlas os -> Size2
atlasSize (MkFont (_, _, _, size, _)) = size

textInit :: IO FT_Library
textInit = ft_Init_FreeType -- any additional initialization goes here

textFinish :: FT_Library -> IO ()
textFinish = ft_Done_FreeType

loadFont :: (ContextHandler ctx, MonadIO m) => FT_Library -> FilePath -> Size2 -> ContextT ctx os m (FontAtlas os)
loadFont ft path size = do
                            face  <- liftIO $ ft_New_Face ft path 0
                            liftIO $ ft_Set_Char_Size face 0 (20*64) 0 0
                            atlas <- newTexture2D R8 size 1
                            writeTexture2D atlas 0 0 size $ repeat (0 :: Float)
                            pure $ MkFont (face, HM.empty, atlas, size, V3 0 0 0)

unloadFont :: (ContextHandler ctx, MonadIO m) => FontAtlas os -> ContextT ctx os m ()
unloadFont (MkFont (face, _, atlas, _, _)) = do
                                                liftIO $ ft_Done_Face face
                                                --TODO: unload the texture atlas from the GPU
                                                pure ()

loadCharacter :: (ContextHandler ctx, MonadIO m) => FontAtlas os -> Char -> ContextT ctx os m (Maybe (FontAtlas os))
loadCharacter font@(MkFont (face, reg, atlas, size@(V2 atlasW atlasH), V3 offsetX offsetY lineH)) ch 
    = case HM.member ch reg of
        True  -> pure $ Just font
        False -> do 
                    liftIO $ ft_Load_Char face (fromIntegral . fromEnum $ ch) FT_LOAD_RENDER
                    glyphSlot <- liftIO $ peek face >>= peek . frGlyph
                    let metrics = gsrMetrics glyphSlot
                        typography = V3 (fromIntegral . gmHoriBearingX $ metrics) 
                                        (fromIntegral . gmHoriBearingY $ metrics)
                                        (fromIntegral . gmHoriAdvance  $ metrics)
                        bitmap = gsrBitmap glyphSlot
                        width = fromIntegral $ bPitch bitmap
                        height = fromIntegral $ bRows bitmap
                        bitmapSize = V2 width height
                    bitmapBuffer :: [Word8] <- liftIO $ peekArray (width*height) $ bBuffer bitmap
                    let bitmapBufferNormalized :: [Float] = fmap ((/255.0) . fromIntegral) bitmapBuffer
                    case (offsetX + width < atlasW, offsetY + height < atlasH) of
                        (_, False)   -> pure Nothing
                        (True, True) -> do
                                        writeTexture2D atlas 0 (V2 offsetX offsetY) bitmapSize $ bitmapBufferNormalized
                                        let lineH' = max lineH height
                                            reg' = HM.insert ch (toDisplayChar offsetX offsetY width height typography atlasW atlasH) reg
                                        pure . Just $ MkFont (face, reg', atlas, size, V3 (offsetX + width) offsetY lineH')
                        (False, True) -> do
                                        let offsetY' = offsetY + lineH
                                        writeTexture2D atlas 0 (V2 0 offsetY') bitmapSize bitmapBufferNormalized
                                        let reg' = HM.insert ch (toDisplayChar 0 offsetY' width height typography atlasW atlasH) reg
                                        pure . Just $ MkFont (face, reg', atlas, size, V3 width offsetY' height)

toDisplayChar :: Int -> Int -> Int -> Int -> V3 Int -> Int -> Int -> DisplayChar
toDisplayChar offsetX offsetY width height typography atlasW atlasH 
    = MkDisplayChar (V4 (relativeX offsetX) (relativeY offsetY) (relativeX width) (relativeY height), typography')
        where
            relativeX x = fromIntegral x / fromIntegral atlasW
            relativeY y = fromIntegral y / fromIntegral atlasH
            typography' = case typography of
                V3 xBearing yBearing advance -> V3 (fromIntegral xBearing / 64.0) (fromIntegral yBearing / 64.0) (fromIntegral advance / 64.0)

loadCharacters :: (ContextHandler ctx, MonadIO m) => FontAtlas os -> [Char] -> ContextT ctx os m (Maybe (FontAtlas os))
loadCharacters font = foldM go (Just font)
    where 
          go :: (ContextHandler ctx, MonadIO m) => Maybe (FontAtlas os) -> Char -> ContextT ctx os m (Maybe (FontAtlas os))
          go (Just font') = loadCharacter font'
          go Nothing      = const $ pure Nothing

getCharacter :: FontAtlas os -> Char -> Either Char DisplayChar
getCharacter (MkFont (_, reg, _, _, _)) ch 
    = case HM.lookup ch reg of
        Just disp -> Right disp
        Nothing   -> Left ch

getCharacterDefault :: FontAtlas os -> DisplayChar -> Char -> DisplayChar
getCharacterDefault (MkFont (_, reg, _, _, _)) df ch = HM.lookupDefault df ch reg


makeTextSprites :: FontAtlas os -> V3 Float -> Float -> Float -> DisplayChar -> [Char] -> [(V3 Float, V4 Float)] -- 3D-position + atlas offset + atlas chunk size
makeTextSprites font at xResolution yResolution defaultChar = fst . foldr go ([], 0)
    where
         go :: Char -> ([(V3 Float, V4 Float)], Float) -> ([(V3 Float, V4 Float)], Float)
         go ch (sprites, xPos) = case getCharacterDefault font defaultChar ch of
                                MkDisplayChar (atlasPos, V3 xBearing yBearing advance) 
                                    -> ((at + V3 ((xPos + xBearing)/xResolution) (yBearing/yResolution) 0, atlasPos) : sprites, xPos + advance)