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
    applyEvent guiEvent guiState = let elements = guiElements guiState
        in case guiEvent of
            CreateElement preGUIElement -> (createElement preGUIElement guiState, [])
            DeleteElement elementID -> (deleteElement elementID guiState, [])
            MoveElement elementID displacement -> (guiState {guiElements = adjustToTop (move displacement) elementID elements}, [])
            ClickAtGUI click -> case findLast (isInBoundingBox (getCursor click) . getBoundingBox) elements of
                                Nothing -> (guiState, [])
                                Just (MkGUIElement (elementID, V4 x y _ _, eventHandler, _)) -> ( guiState {guiElements = adjustToTop id elementID elements}
                                                                                                , eventHandler elementID $ adjustCursor (subtract $ V2 x y) click
                                                                                                )
            UpdateMisensceneElement preGUIElement -> let guiState' = case misensceneElement guiState of
                                                                        Nothing     ->    guiState
                                                                        Just elementID -> deleteElement elementID guiState
                                                         newID = nextID guiState'
                                                         (bBox, eventHandler, renderHandler) = preGUIElement
                                                      in (guiState' {nextID = newID + 1
                                                                    , guiElements = flip (IM.insert newID) (guiElements guiState') 
                                                                        $ MkGUIElement (newID, bBox, eventHandler, renderHandler)
                                                                    , misensceneElement = Just newID
                                                                    }
                                                         , []
                                                         )

createElement :: PreGUIElement Event -> GUIState Event -> GUIState Event
createElement (bBox, eventHandler, renderHandler) guiState 
        = let newID = nextID guiState 
          in guiState {nextID = newID + 1
                      , guiElements = flip (IM.insert newID) (guiElements guiState) $ MkGUIElement (newID, bBox, eventHandler, renderHandler)
                      }

deleteElement :: Int -> GUIState Event -> GUIState Event
deleteElement elementID guiState = guiState {guiElements = IM.delete elementID $ guiElements guiState}

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