{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, MultiParamTypeClasses#-}   
module Events (Event(..), StateEvent(..), splitEvents, update) where

--
import                         GUI.GUIState
import                         Board.MapState

data Event = Event'GUIEvent (GUIEvent Event)
           | Event'MapEvent  MapEvent
        -- | Event'LogicEvent (LogicEvent Event)

class StateEvent s e where
    applyEvent :: e -> s -> (s, [Event])

update :: (StateEvent s e, Foldable t) => s -> t e -> (s, [Event])
update initialState = foldr go (initialState, [])
    where go event (state, events) = (events++) <$> applyEvent event state

splitEvents :: [Event] -> ([GUIEvent Event], [MapEvent])
splitEvents = foldr go ([], [])
    where go (Event'GUIEvent guiEvent) (xs, ys) = (guiEvent: xs, ys)
          go (Event'MapEvent mapEvent) (xs, ys) = (xs, mapEvent : ys)