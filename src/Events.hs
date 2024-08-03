{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, MultiParamTypeClasses#-}   
module Events (processEvents, Event(..), StateEvent(..), splitEvents, update) where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW (Key(..), MouseButton(..))
--
import                         Constants
import                         Input
import                         Projection                          (projectCursor)
import                         GUI.GUIState
import                         Board.MapState

data Event = Event'GUIEvent (GUIEvent Event)
           | Event'MapEvent  MapEvent
        -- | Event'LogicEvent (LogicEvent Event)

processEvents :: MapState -> Input -> [Event]
processEvents mapState input = [ Event'MapEvent $ MoveCamera pos'
                               , Event'MapEvent $ UpdateMapCursor cursor
                               ] ++ mouseEvents
    where keyboardMovement = case keyboardInput input of
                                    Right charInput -> foldr processKeys (V3 0 0 0) charInput
                                    Left textInput  -> V3 0 0 0
          processKeys GLFW.Key'W v = v + V3  0                    (negate cameraSpeed) 0
          processKeys GLFW.Key'A v = v + V3  (negate cameraSpeed) 0                    0
          processKeys GLFW.Key'S v = v + V3  0                    cameraSpeed          0
          processKeys GLFW.Key'D v = v + V3  cameraSpeed          0                    0
          processKeys GLFW.Key'PadAdd      v = v + V3 0 0 (negate . realToFrac $ (zoomSpeed * keyScrollSensitivity))
          processKeys GLFW.Key'PadSubtract v = v + V3 0 0 (realToFrac $ (zoomSpeed * keyScrollSensitivity))
          processKeys _ diff   = diff
          dPos = keyboardMovement + V3 0 0 (negate . realToFrac $ (scrollInput input * mouseScrollSensitivity * zoomSpeed))
          pos'@(V3 x z zoom) = boundZoom (position mapState + normDPos dPos)
          (mouseEvents, cursor) = case cursorPosition input of
            Right cursorOnMap -> let projectedCursor = projectCursor (V3 x 0 z) zoom $ cursorOnMap 
                                 in (processMouseOnMap projectedCursor $ mouseButtonInput input, Just $ projectedCursor)
            Left  cursorOnGUI -> (processMouseOnGUI cursorOnGUI $ mouseButtonInput input, Nothing)
            
boundZoom :: V3 Float -> V3 Float
boundZoom (V3 x y s) = V3 x y s'
    where s' = max s zoomMinBound

normDPos :: V3 Float -> V3 Float
normDPos dPos@(V3 0 0 s) = dPos
normDPos (V3 x y s) = V3 x' y' s
    where (V2 x' y') = cameraSpeed *^ (signorm $ V2 x y)

processMouseOnMap :: V4 Float -> [GLFW.MouseButton] -> [Event]
processMouseOnMap cursor = foldr go []
    where go _ xs = xs

processMouseOnGUI :: V2 Int -> [GLFW.MouseButton] -> [Event]
processMouseOnGUI cursor = foldr go []
    where go GLFW.MouseButton'1 xs = Event'GUIEvent (ClickAt $ GUILeftClick cursor) : xs
          go GLFW.MouseButton'2 xs = Event'GUIEvent (ClickAt $ GUIRightClick cursor) : xs
          go _                  xs = xs

class StateEvent s e where
    applyEvent :: e -> s -> (s, [Event])

update :: (StateEvent s e, Foldable t) => s -> t e -> (s, [Event])
update initialState = foldr go (initialState, [])
    where go event (state, events) = (events++) <$> applyEvent event state

splitEvents :: [Event] -> ([GUIEvent Event], [MapEvent])
splitEvents = foldr go ([], [])
    where go (Event'GUIEvent guiEvent) (xs, ys) = (guiEvent: xs, ys)
          go (Event'MapEvent mapEvent) (xs, ys) = (xs, mapEvent : ys)