{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module GUI.UpdateGUI where

import                         Graphics.GPipe
import qualified               Data.HashMap.Strict.InsOrd as IM
import                         Data.HashMap.Strict.InsOrd        (InsOrdHashMap)
import                         Data.Hashable                     (Hashable)
import                         Data.Monoid                       (Last(..))
--
import                         GUI.GUIState
import                         Events


instance StateEvent (GUIState Event) (GUIEvent Event) where
    applyEvent guiEvent guistate@(MkGUIState (nextID, elements)) = case guiEvent of
            CreateElement (bBox, eventHandler, renderHandler) 
                -> (MkGUIState (nextID + 1, flip (IM.insert nextID) elements $ MkGUIElement (nextID, bBox, eventHandler, renderHandler)), [])
            DeleteElement elementID -> (MkGUIState (nextID, IM.delete elementID elements), [])
            MoveElement elementID displacement -> (MkGUIState (nextID, adjustToTop (move displacement) elementID elements), [])
            ClickAtGUI click -> case findLast (isInBoundingBox (getCursor click) . getBoundingBox) elements of
                                Nothing -> (guistate, [])
                                Just (MkGUIElement (elementID, V4 x y _ _, eventHandler, _)) -> ( MkGUIState (nextID, adjustToTop id elementID elements)
                                                                                                , eventHandler elementID $ adjustCursor (subtract $ V2 x y) click
                                                                                                )

-- Helper functions --
adjustToTop :: (Eq k, Hashable k) => (v -> v) -> k -> InsOrdHashMap k v -> InsOrdHashMap k v
adjustToTop f key imap = case IM.lookup key imap of
                                Just val -> IM.insert key (f val) . IM.delete key $ imap
                                Nothing  -> imap

move :: V2 Int -> GUIElement event -> GUIElement event
move (V2 x y) (MkGUIElement (elemID, bBox, eventHandler, renderHandler)) = MkGUIElement (elemID, bBox + V4 x y 0 0, eventHandler, renderHandler)

-- verbatim from Data.Monoid, but First replaced with Last
findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast p = getLast . foldMap (\x -> Last (if p x then Just x else Nothing))