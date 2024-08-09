{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Main where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW
import                         Control.Monad                        (unless)
import                         Control.Monad.IO.Class               (MonadIO(..), liftIO)
--
import                         Constants
import                         Texture
import                         Input
import                         Events
import                         ProcessEvents
import                         Text
import                         Board.RenderBoard                    (initBoardRenderer)
import                         Board.MapState                       
import                         Board.UpdateMap                      () -- instances
import                         GUI.RenderGUI                        (initGUIRenderer)
import                         GUI.GUIState
import                         GUI.UpdateGUI                        () -- instances
import                         GUI.GUIElements


main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGBA8) 
        (GLFW.WindowConfig displayWidth displayHeight "The Negotiation Game" Nothing [GLFW.WindowHint'Resizable False] Nothing)
    
    mapTextureArray <- importTextureArray [mapPath, provinceMapPath] mapHeight mapWidth
    
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
    (preRenderBoard, renderBoard) <- initBoardRenderer initialPosition win mapTextureArray
    (preRenderGUI, renderGUI) <- initGUIRenderer win atlas
    
    provinceMap <- liftIO $ getImage provinceMapPath
    let mapState = MkMapState { position     = initialPosition
                              , cursor       = Just (V4 0 0 0 1)
                              , mapMode      = RawMapMode
                              , entityMap    = provinceMap
                              , selectedProv = Nothing
                              }
        (guiState, _) = applyEvent (CreateElement cursorStatusPreElement) (blankGUIState :: GUIState Event) 
    
    maybeTime <- liftIO GLFW.getTime
    let startingTime = case maybeTime of
                        Just time -> time
                        Nothing   -> 0
    
    gameLoop renderBoard preRenderBoard
             renderGUI preRenderGUI
             win
             mapState guiState []
             inputTVars startingTime 0
             defaultFont' defaultChar
             cleanupCallback


gameLoop renderBoard preRenderBoard
         renderGUI preRenderGUI
         win
         mapState (guiState :: GUIState Event) pendingEvents
         inputTVars previousTime previousExcessTime
         defaultFont defaultChar
         cleanupCallback = do
    -- Collect input --
    maybeCurrentTime <- liftIO GLFW.getTime
    input <- collectInput win inputTVars (flip isInsideGUI $ guiState)
    let currentEvents = processEvents mapState $ input
    
    -- Process events arising from input --
    let currentTime = case maybeCurrentTime of
                                Just x  -> x
                                Nothing -> previousTime
        accumulatedTime = previousExcessTime + (currentTime - previousTime)
        (excessTime, pendingEvents', guiState', mapState') = if accumulatedTime > logicUpdateTime
            then let (guiEvents, mapEvents) = splitEvents . (pendingEvents ++) $ currentEvents
                     (guiStateUpdated, pendingGUIEvents) = update guiState guiEvents
                     (mapStateUpdated, pendingMapEvents) = update mapState mapEvents
                 in (accumulatedTime - logicUpdateTime, pendingGUIEvents ++ pendingMapEvents, guiStateUpdated, mapStateUpdated)
            else (accumulatedTime, currentEvents ++ pendingEvents, guiState, mapState)
    
    let cursorPointer = fmap normalizePoint $ cursor mapState'
    
    preRenderBoard mapState'
    (textLength, boxNumber) <- preRenderGUI (cursorPosition input) cursorPointer defaultFont defaultChar guiState'
        
    -- Render the state --
    render $ do
        clearWindowColor win (V4 1 1 1 1)
        renderBoard
        renderGUI textLength boxNumber
    
    --liftIO . putStrLn . show $ (currentTime - previousTime) * 1000 -- for debug purposes
    
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
                 mapState' guiState' pendingEvents'
                 inputTVars currentTime excessTime
                 defaultFont defaultChar
                 cleanupCallback
                 
cleanup freetype defaultFont = do
                                unloadFont defaultFont
                                liftIO $ textFinish freetype