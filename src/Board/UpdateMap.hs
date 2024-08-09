{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Board.UpdateMap where

import                         Graphics.GPipe
import                         Data.Ix              (inRange)
--
import                         Constants
import                         Board.MapState
import                         Events
import                         Texture              (getColorAt)
import                         GUI.GUIState         (GUIEvent(..))
import                         GUI.GUIElements      (provinceWindowPreElement)

instance StateEvent MapState MapEvent where
    applyEvent mapEvent mapState = case mapEvent of
            MoveCamera displacement -> (mapState {position = boundZoom (position mapState + displacement)}, [])
            UpdateMapCursor newCursor -> (mapState {cursor = newCursor}, [])
            UpdateMapMode newMapMode -> (mapState {mapMode = newMapMode}, [])
            ClickAtMap click -> case click of
                MapLeftClick (V3 x y z) -> let mapX = round $ x * (mapWidth / mapQuadWidth)
                                               mapY = round $ z * (mapHeight / mapQuadHeight)
                                               colorAt = getColorAt (entityMap mapState) mapX mapY
                                           in case inRange ((0, 0), (mapWidth, mapHeight)) (mapX, mapY) of
                                                True  -> (  mapState {selectedProv = Just colorAt}
                                                         , [Event'GUIEvent $ UpdateMisensceneElement $ provinceWindowPreElement colorAt]
                                                         )
                                                False -> (mapState, [])
                MapRightClick _ -> (mapState, [])
            UnselectProvince -> (mapState {selectedProv = Nothing}, [])
            _  -> (mapState, [])

boundZoom :: V3 Float -> V3 Float
boundZoom (V3 x y s) = V3 x y s'
    where s' = max s zoomMinBound