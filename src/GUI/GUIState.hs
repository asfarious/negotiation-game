module GUI.GUIState where

import                         Graphics.GPipe
import qualified               Data.HashMap.Strict.InsOrd as IM
import                         Data.HashMap.Strict.InsOrd        (InsOrdHashMap)
import                         Data.Ix                           (inRange)
import                         Data.Monoid                       (mconcat, Any(..))

blankGUIState :: GUIState event
blankGUIState = MkGUIState { nextID            = 0
                           , guiElements       = IM.empty
                           , misensceneElement = Nothing
                           }

data GUIState event = MkGUIState { nextID            :: Int
                                 , guiElements       :: InsOrdHashMap Int (GUIElement event)
                                 , misensceneElement :: Maybe Int
                                 }

newtype GUIElement event = MkGUIElement (Int, BoundingBox, GUIEventHandler event, GUIRenderHandler)

type GUIEventHandler event = Int -> GUIClick -> [event]
type GUIRenderHandler = BoundingBox -> (Either (V2 Int) (V2 Float), Maybe (V3 Float)) -> [GUIBox]
type PreGUIElement event = (BoundingBox, GUIEventHandler event, GUIRenderHandler)

getBoundingBox :: GUIElement event -> BoundingBox
getBoundingBox (MkGUIElement (_, bBox, _, _)) = bBox

type BoundingBox = V4 Int -- position and size in pixels

data GUIBox = ColoredBox BoundingBox (V4 Float)
            | TextBox BoundingBox String 
            deriving Show

data GUIEvent event = CreateElement (PreGUIElement event)
                    | DeleteElement Int
                    | MoveElement Int (V2 Int)
                    | ClickAtGUI GUIClick
                    | UpdateMisensceneElement (PreGUIElement event)

data GUIClick = GUILeftClick (V2 Int)
              | GUIRightClick (V2 Int)

getCursor :: GUIClick -> V2 Int
getCursor (GUILeftClick cursor) = cursor
getCursor (GUIRightClick cursor) = cursor

adjustCursor :: (V2 Int -> V2 Int) -> GUIClick -> GUIClick
adjustCursor f (GUILeftClick cursor) = GUILeftClick (f cursor)
adjustCursor f (GUIRightClick cursor) = GUIRightClick (f cursor)

isInBoundingBox :: V2 Int -> BoundingBox -> Bool
isInBoundingBox (V2 x y) (V4 boxX boxY boxWidth boxHeight) = inRange ((boxX, boxY), (boxX + boxWidth, boxY + boxHeight)) (x, y)

isInsideGUI :: V2 Int -> GUIState event -> Bool
isInsideGUI pos guiState = getAny . IM.unorderedFoldMap (Any . isInBoundingBox pos . getBoundingBox) $ guiElements guiState