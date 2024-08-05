module Board.MapState where

import                         Graphics.GPipe


data MapState = MkMapState { position :: (V3 Float)
                           , cursor   :: Maybe (V4 Float)
                           , mapMode  :: MapMode
                           }

data MapMode = RawMapMode

data MapEvent = MoveCamera (V3 Float)
              | UpdateMapCursor (Maybe (V4 Float))
              | UpdateMapMode MapMode
              | ClickAtMap MapClick
              
data MapClick = MapLeftClick (V3 Float)
              | MapRightClick (V3 Float)