{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, MultiParamTypeClasses#-}   
module Events (processEvents, Event(..), StateEvent(..), splitEvents, update) where

import                         Graphics.GPipe
import qualified "GPipe-GLFW4" Graphics.GPipe.Context.GLFW    as GLFW (Key(..), MouseButton(..))
import                         Control.Monad.Trans.Writer.CPS
import                         Data.Foldable                         (traverse_)
--
import                         Constants
import                         Input
import                         Projection                             (projectCursor)
import                         GUI.GUIState
import                         Board.MapState

data Event = Event'GUIEvent (GUIEvent Event)
           | Event'MapEvent  MapEvent
        -- | Event'LogicEvent (LogicEvent Event)

type CreateEvents = Writer [Event]
tellP = tell . pure

processEvents :: MapState -> Input -> [Event]
processEvents mapState input = execWriter $ do
        let keyboardMovement = case keyboardInput input of
                                    Right charInput -> foldr processKeys (V3 0 0 0) charInput
                                    Left textInput  -> V3 0 0 0
            dPos = keyboardMovement + V3 0 0 (negate . realToFrac $ (scrollInput input * mouseScrollSensitivity * zoomSpeed))
            pos'@(V3 x z zoom) = boundZoom (position mapState + normDPos dPos)
            
        if pos' /= V3 0 0 0
            then tellP $ Event'MapEvent $ MoveCamera pos'
            else pure ()
        
        cursor <- case cursorPosition input of
                    Right cursorOnMap -> do
                                            let projectedCursor = projectCursor (V3 x 0 z) zoom $ cursorOnMap 
                                            processMouseOnMap projectedCursor $ mouseButtonInput input 
                                            pure $ Just $ projectedCursor
                    Left  cursorOnGUI -> do
                                            processMouseOnGUI cursorOnGUI $ mouseButtonInput input
                                            pure Nothing
        
        tellP $ Event'MapEvent $ UpdateMapCursor cursor
    
processKeys GLFW.Key'W v = v + V3  0                    (negate cameraSpeed) 0
processKeys GLFW.Key'A v = v + V3  (negate cameraSpeed) 0                    0
processKeys GLFW.Key'S v = v + V3  0                    cameraSpeed          0
processKeys GLFW.Key'D v = v + V3  cameraSpeed          0                    0
processKeys GLFW.Key'PadAdd      v = v + V3 0 0 (negate . realToFrac $ (zoomSpeed * keyScrollSensitivity))
processKeys GLFW.Key'PadSubtract v = v + V3 0 0 (realToFrac $ (zoomSpeed * keyScrollSensitivity))
processKeys _ diff   = diff
          
          
            
boundZoom :: V3 Float -> V3 Float
boundZoom (V3 x y s) = V3 x y s'
    where s' = max s zoomMinBound

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

class StateEvent s e where
    applyEvent :: e -> s -> (s, [Event])

update :: (StateEvent s e, Foldable t) => s -> t e -> (s, [Event])
update initialState = foldr go (initialState, [])
    where go event (state, events) = (events++) <$> applyEvent event state

splitEvents :: [Event] -> ([GUIEvent Event], [MapEvent])
splitEvents = foldr go ([], [])
    where go (Event'GUIEvent guiEvent) (xs, ys) = (guiEvent: xs, ys)
          go (Event'MapEvent mapEvent) (xs, ys) = (xs, mapEvent : ys)