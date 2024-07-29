{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Main where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (unless)
import                         Control.Monad.IO.Class               (MonadIO(..), liftIO)
import                         Control.Concurrent.STM
--
import                         Constants
import                         Texture
import                         Input
import                         States
import                         Events
import                         Text
import                         RenderBoard                          (initBoardRenderer)
import                         RenderGUI                            (initGUIRenderer)


main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGBA8) 
        (GLFW.WindowConfig displayWidth displayHeight "The Negotiation Game" Nothing [GLFW.WindowHint'Resizable False] Nothing)
    
    mapTexture <- importTexture mapPath mapHeight mapWidth
    
    freetype <- liftIO $ textInit
    defaultFont <- loadFont freetype defaultFontFile (V2 mapWidth mapHeight) 
    maybeDefaultFont' <- loadCharacters defaultFont ("#.,()!" ++ ['0'..'9'] ++ ['A'..'Z'] ++ "?" ++ ['А'..'Я'] ++ ['a'..'z'] ++ ['а'..'я'] ++ " +-_=:;")
    let defaultFont' = case maybeDefaultFont' of
                        Just font -> font
                        Nothing   -> error "FONT ERROR!!!"
        defaultChar = case getCharacter defaultFont' '#' of
                        Right octothorp -> octothorp
                        Left _          -> error "# NOT LOADED SUCCESSFULLY"
    let atlas = atlasTexture defaultFont'
    
    let cleanupCallback = cleanup freetype
    
    isStrTVar'   :: TVar Bool       <- liftIO $ atomically $ newTVar (False :: Bool)
    strTVar'    :: TVar String      <- liftIO $ atomically $ newTVar ([] :: String)
    scrollTVar' :: TVar Double      <- liftIO $ atomically $ newTVar (0 :: Double)
    let inputTVars = MkInputTVars { scrollTVar = scrollTVar'
                                  , strTVar    = strTVar'
                                  , isStrTVar  = isStrTVar'
                                  }
    GLFW.setScrollCallback win $ Just $ mouseScrollCallback scrollTVar'
    GLFW.setCharCallback win $ Just $ keyboardCallback strTVar' isStrTVar'
    
    let initialPosition = V3 (mapQuadWidth/2) (mapQuadHeight/2) initialZoom
    (preRenderBoard, renderBoard) <- initBoardRenderer initialPosition win mapTexture
    (preRenderGUI, renderGUI) <- initGUIRenderer win atlas
    
    let mapState = MkMapState { position = initialPosition
                              , cursor   = Just (V4 0 0 0 1)
                              , mapMode  = RawMapMode
                              }
                              
    gameLoop renderBoard preRenderBoard
             renderGUI preRenderGUI
             win
             mapState 
             inputTVars 
             defaultFont' defaultChar
             cleanupCallback


gameLoop renderBoard preRenderBoard
         renderGUI preRenderGUI
         win
         mapState 
         inputTVars 
         defaultFont defaultChar
         cleanupCallback = do
    -- Collect input --
    input <- collectInput win inputTVars

    -- Process events arising from input --
    let (mapState', debugIO) = processEvents mapState input
    _ <- liftIO $ debugIO
    
    let cursorPointer = case cursor mapState of
                                Just cur -> normalizePoint cur
                                Nothing  -> V3 0 0 0
    
    preRenderBoard mapState'
    textLength <- preRenderGUI (cursorPosition input) cursorPointer defaultFont defaultChar
        
    -- Render the state --
    render $ do
        clearWindowColor win (V4 1 1 1 1)
        renderBoard
        renderGUI textLength
  
    swapWindowBuffers win
    
    -- Do another iteration --
    closeRequested <- GLFW.windowShouldClose win
    if (closeRequested == Just True) -- I hope this strange way to do cleanup helps GHC to optimize the recursion into a loop
        then cleanupCallback defaultFont
        else pure ()
        
    unless (closeRequested == Just True) $ 
        gameLoop renderBoard preRenderBoard
                 renderGUI preRenderGUI
                 win
                 mapState' 
                 inputTVars 
                 defaultFont defaultChar
                 cleanupCallback
                 
cleanup freetype defaultFont = do
                                unloadFont defaultFont
                                liftIO $ textFinish freetype