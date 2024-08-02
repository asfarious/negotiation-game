module GUI.GUIState where

import                         Graphics.GPipe
import qualified               Data.HashMap.Strict.InsOrd as IM
import                         Data.HashMap.Strict.InsOrd        (InsOrdHashMap)
import                         Data.Ix                           (inRange)
import                         Data.Monoid                       (mconcat, Any(..))

blankGUIState :: GUIState event
blankGUIState = MkGUIState (0, IM.empty)

newtype GUIState event = MkGUIState (Int, InsOrdHashMap Int (GUIElement event))

newtype GUIElement event = MkGUIElement (Int, BoundingBox, Int -> V2 Int -> [event], BoundingBox -> (Either (V2 Int) (V2 Float), Maybe (V3 Float)) -> [GUIBox])

getBoundingBox :: GUIElement event -> BoundingBox
getBoundingBox (MkGUIElement (_, bBox, _, _)) = bBox

type BoundingBox = V4 Int -- position and size in pixels

data GUIBox = ColoredBox BoundingBox (V4 Float)
            | TextBox BoundingBox String 
            deriving Show

data GUIEvent event = CreateElement (PreGUIElement event)
                    | DeleteElement Int
                    | MoveElement Int (V2 Int)

type PreGUIElement event = (BoundingBox, Int -> V2 Int -> [event], BoundingBox -> (Either (V2 Int) (V2 Float), Maybe (V3 Float)) -> [GUIBox])

isInBoundingBox :: V2 Int -> BoundingBox -> Bool
isInBoundingBox (V2 x y) (V4 boxX boxY boxWidth boxHeight) = inRange ((x, y), (boxX + boxWidth, boxY + boxHeight)) (x, y)

isInsideGUI :: V2 Int -> GUIState event -> Bool
isInsideGUI pos (MkGUIState (_, elements)) = getAny . IM.unorderedFoldMap (Any . isInBoundingBox pos . getBoundingBox) $ elements