module GUI.UpdateGUI where

import                         Graphics.GPipe
import qualified               Data.HashMap.Strict.InsOrd as IM
import                         Data.HashMap.Strict.InsOrd        (InsOrdHashMap)
import                         Data.Hashable                     (Hashable)
--
import                         GUI.GUIState
import                         Events

updateGUI :: GUIState Event -> [GUIEvent Event] -> GUIState Event
updateGUI = foldr applyEvent

applyEvent :: GUIEvent Event -> GUIState Event -> GUIState Event
applyEvent guiEvent guistate@(MkGUIState (nextID, elements)) = case guiEvent of
        CreateElement (bBox, eventHandler, renderHandler) 
            -> MkGUIState (nextID + 1, flip (IM.insert nextID) elements $ MkGUIElement (nextID, bBox, eventHandler, renderHandler))
        DeleteElement elementID -> MkGUIState (nextID, IM.delete elementID elements)
        MoveElement elementID displacement -> MkGUIState (nextID, adjustToTop (move displacement) elementID elements)


-- Helper functions --
adjustToTop :: (Eq k, Hashable k) => (v -> v) -> k -> InsOrdHashMap k v -> InsOrdHashMap k v
adjustToTop f key imap = case IM.lookup key imap of
                                Just val -> IM.insert key (f val) . IM.delete key $ imap
                                Nothing  -> imap

move :: V2 Int -> GUIElement event -> GUIElement event
move (V2 x y) (MkGUIElement (elemID, bBox, eventHandler, renderHandler)) = MkGUIElement (elemID, bBox + V4 x y 0 0, eventHandler, renderHandler)