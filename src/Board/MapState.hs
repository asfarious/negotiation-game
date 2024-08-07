module Board.MapState where

import                         Graphics.GPipe
import qualified               Codec.Picture as JP    (Image, PixelRGBA8)

data MapState = MkMapState { position     :: (V3 Float)
                           , cursor       :: Maybe (V4 Float)
                           , mapMode      :: MapMode
                           , entityMap    :: JP.Image JP.PixelRGBA8
                           , selectedProv :: Maybe (V4 Float)
                           }

data MapMode = RawMapMode

data MapEvent = MoveCamera (V3 Float)
              | UpdateMapCursor (Maybe (V4 Float))
              | UpdateMapMode MapMode
              | ClickAtMap MapClick
              | UnselectProvince
              
data MapClick = MapLeftClick (V3 Float)
              | MapRightClick (V3 Float)