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
import                         Events
import                         Text
import                         Board.RenderBoard                    (initBoardRenderer)
import                         Board.MapState                       
import                         Board.UpdateMap
import                         GUI.RenderGUI                        (initGUIRenderer)
import                         GUI.GUIState
import                         GUI.UpdateGUI
import                         GUI.GUIElements


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
    
    inputTVars <- initInput win
    
    let initialPosition = V3 (mapQuadWidth/2) (mapQuadHeight/2) initialZoom
    (preRenderBoard, renderBoard) <- initBoardRenderer initialPosition win mapTexture
    (preRenderGUI, renderGUI) <- initGUIRenderer win atlas
    
    let mapState = MkMapState { position = initialPosition
                              , cursor   = Just (V4 0 0 0 1)
                              , mapMode  = RawMapMode
                              }
        (guiState, _) = applyEvent (CreateElement cursorStatusPreElement) (blankGUIState :: GUIState Event) 
                              
    gameLoop renderBoard preRenderBoard
             renderGUI preRenderGUI
             win
             mapState guiState []
             inputTVars 
             defaultFont' defaultChar
             cleanupCallback


gameLoop renderBoard preRenderBoard
         renderGUI preRenderGUI
         win
         mapState (guiState :: GUIState Event) pendingEvents
         inputTVars 
         defaultFont defaultChar
         cleanupCallback = do
    -- Collect input --
    input <- collectInput win inputTVars (flip isInsideGUI $ guiState)
    
    -- Process events arising from input --
    let (guiEvents, mapEvents) = splitEvents . (pendingEvents ++) . processEvents mapState $ input
    let (guiState', pendingGUIEvents) = update guiState guiEvents
    let (mapState', pendingMapEvents) = update mapState mapEvents
    
    let cursorPointer = fmap normalizePoint $ cursor mapState'
    
    preRenderBoard mapState'
    (textLength, boxNumber) <- preRenderGUI (cursorPosition input) cursorPointer defaultFont defaultChar guiState'
        
    -- Render the state --
    render $ do
        clearWindowColor win (V4 1 1 1 1)
        renderBoard
        renderGUI textLength boxNumber
  
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
                 mapState' guiState' (pendingMapEvents ++ pendingGUIEvents)
                 inputTVars 
                 defaultFont defaultChar
                 cleanupCallback
                 
cleanup freetype defaultFont = do
                                unloadFont defaultFont
                                liftIO $ textFinish freetype