{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Board.UpdateMap where

import                         Graphics.GPipe
--
import                         Constants
import                         Board.MapState
import                         Events
import                         Texture              (getColorAt)
import                         GUI.GUIState         (GUIEvent(..))
import                         GUI.GUIElements      (provinceWindowPreElement)

instance StateEvent MapState MapEvent where
    applyEvent mapEvent mapState = case mapEvent of
            MoveCamera newPosition -> (mapState {position = newPosition}, [])
            UpdateMapCursor newCursor -> (mapState {cursor = newCursor}, [])
            UpdateMapMode newMapMode -> (mapState {mapMode = newMapMode}, [])
            ClickAtMap click -> case click of
                MapLeftClick (V3 x y z) -> let mapX = round $ x * (mapWidth / mapQuadWidth)
                                               mapY = round $ z * (mapHeight / mapQuadHeight)
                                               colorAt = getColorAt (entityMap mapState) mapX mapY
                                           in (mapState {selectedProv = Just colorAt}, [Event'GUIEvent $ CreateElement $ provinceWindowPreElement colorAt])
                MapRightClick _ -> (mapState, [])
            UnselectProvince -> (mapState {selectedProv = Nothing}, [])
            _  -> (mapState, [])