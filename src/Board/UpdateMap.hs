{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Board.UpdateMap where

import                         Graphics.GPipe
--
import                         Board.MapState
import                         Events

instance StateEvent MapState MapEvent where
    applyEvent mapEvent mapState = case mapEvent of
            MoveCamera newPosition -> (mapState {position = newPosition}, [])
            UpdateMapCursor newCursor -> (mapState {cursor = newCursor}, [])
            UpdateMapMode newMapMode -> (mapState {mapMode = newMapMode}, [])