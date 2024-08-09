{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies#-}   
module ProcessEvents (processEvents) where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW    as GLFW (Key(..), MouseButton(..))
import                         Control.Monad.Trans.Writer.CPS
import                         Data.Foldable                         (traverse_)
--
import                         Constants
import                         Events
import                         Input
import                         Projection                             (projectCursor)
import                         GUI.GUIState
import                         Board.MapState
import                         GUI.GUIElements

type CreateEvents = Writer [Event]
tellP = tell . pure

processEvents :: MapState -> Input -> [Event]
processEvents mapState input = execWriter $ do
        keyboardMovement <- case keyboardInput input of
                                                Right (pressedKeys, heldKeys) -> do
                                                                                   tell $ foldr processPressedKeys [] pressedKeys
                                                                                   pure $ foldr processHeldKeys (V3 0 0 0) heldKeys
                                                Left textInput  -> pure $ V3 0 0 0
        let dPos = normDPos $ keyboardMovement + V3 0 0 (negate . realToFrac $ (scrollInput input * mouseScrollSensitivity * zoomSpeed))
        
        if dPos /= V3 0 0 0
            then tellP $ Event'MapEvent $ MoveCamera dPos
            else pure ()
        
        let (V3 x z zoom) = position mapState
        
        cursor <- case cursorPosition input of
                    Right cursorOnMap -> do
                                            let projectedCursor = projectCursor (V3 x 0 z) zoom $ cursorOnMap 
                                            processMouseOnMap projectedCursor $ mouseButtonInput input 
                                            pure $ Just $ projectedCursor
                    Left  cursorOnGUI -> do
                                            processMouseOnGUI cursorOnGUI $ mouseButtonInput input
                                            pure Nothing
        
        tellP $ Event'MapEvent $ UpdateMapCursor cursor

processPressedKeys :: GLFW.Key -> [Event] -> [Event]
processPressedKeys GLFW.Key'C events = Event'GUIEvent (CreateElement cursorStatusPreElement) : events
processPressedKeys _ events          = events

processHeldKeys :: GLFW.Key -> V3 Float -> V3 Float    
processHeldKeys GLFW.Key'W v = v + V3  0                    (negate cameraSpeed) 0
processHeldKeys GLFW.Key'A v = v + V3  (negate cameraSpeed) 0                    0
processHeldKeys GLFW.Key'S v = v + V3  0                    cameraSpeed          0
processHeldKeys GLFW.Key'D v = v + V3  cameraSpeed          0                    0
processHeldKeys GLFW.Key'PadAdd      v = v + V3 0 0 (negate . realToFrac $ (zoomSpeed * keyScrollSensitivity))
processHeldKeys GLFW.Key'PadSubtract v = v + V3 0 0 (realToFrac $ (zoomSpeed * keyScrollSensitivity))
processHeldKeys _ diff = diff
          

normDPos :: V3 Float -> V3 Float
normDPos dPos@(V3 0 0 s) = dPos
normDPos (V3 x y s) = V3 x' y' s
    where (V2 x' y') = cameraSpeed *^ (signorm $ V2 x y)

processMouseOnMap :: V4 Float -> [GLFW.MouseButton] -> CreateEvents ()
processMouseOnMap cursor = traverse_ $ \button -> do
        let normalizedCursor = normalizePoint cursor
        case button of
            GLFW.MouseButton'1 -> tellP . Event'MapEvent . ClickAtMap . MapLeftClick  $ normalizedCursor
            GLFW.MouseButton'2 -> tellP . Event'MapEvent . ClickAtMap . MapRightClick $ normalizedCursor
            _                  -> pure ()

processMouseOnGUI :: V2 Int -> [GLFW.MouseButton] -> CreateEvents ()
processMouseOnGUI cursor = traverse_ $ \button -> do
        case button of
            GLFW.MouseButton'1 -> tellP . Event'GUIEvent . ClickAtGUI . GUILeftClick  $ cursor
            GLFW.MouseButton'2 -> tellP . Event'GUIEvent . ClickAtGUI . GUIRightClick $ cursor
            _                  -> pure ()
