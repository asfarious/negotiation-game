{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}   
module Events (processEvents, Event(..)) where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW as GLFW (Key(..), MouseButton(..))
import                         Constants
import                         States
import                         Input
import                         Projection                          (projectCursor)
import                         GUI.GUIState

data Event = Event'GUIEvent (GUIEvent Event)
        -- | Event'MapEvent (MapEvent Event)
        -- | Event'LogicEvent (LogicEvent Event)

processEvents :: MapState -> Input -> (MapState, IO ())
processEvents (MkMapState pos _ mMode) input = (MkMapState pos' cursor mMode, putStr debug)
    where (keyboardMovement, debug) = case keyboardInput input of
                                        Right charInput -> foldr go (V3 0 0 0, []) charInput
                                        Left textInput  -> (V3 0 0 0, [])
          go GLFW.Key'W (v, debug) = (v + V3  0                    (negate cameraSpeed) 0,                              debug)
          go GLFW.Key'A (v, debug) = (v + V3  (negate cameraSpeed) 0                    0,                              debug)
          go GLFW.Key'S (v, debug) = (v + V3  0                    cameraSpeed          0,                              debug)
          go GLFW.Key'D (v, debug) = (v + V3  cameraSpeed          0                    0,                              debug)
          go GLFW.Key'PadAdd (v, debug)      = (v + V3  0 0 (negate . realToFrac $ (zoomSpeed * keyScrollSensitivity)), debug)
          go GLFW.Key'PadSubtract (v, debug) = (v + V3  0 0 (realToFrac $ (zoomSpeed * keyScrollSensitivity)),          debug)
          go GLFW.Key'M (v, _)     = (v, show pos ++ "\n")
          go _ diff   = diff
          dPos = keyboardMovement + V3 0 0 (negate . realToFrac $ (scrollInput input * mouseScrollSensitivity * zoomSpeed))
          pos'@(V3 x z zoom) = boundZoom (pos + normDPos dPos)
          cursor = case cursorPosition input of
            Right cursorOnMap -> Just $ projectCursor (V3 x 0 z) zoom $ cursorOnMap
            Left  _           -> Nothing
            
boundZoom :: V3 Float -> V3 Float
boundZoom (V3 x y s) = V3 x y s'
    where s' = max s zoomMinBound

normDPos :: V3 Float -> V3 Float
normDPos dPos@(V3 0 0 s) = dPos
normDPos (V3 x y s) = V3 x' y' s
    where (V2 x' y') = cameraSpeed *^ (signorm $ V2 x y)