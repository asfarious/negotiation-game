module States where

import                         Graphics.GPipe

data MapState = MkMapState { position :: (V3 Float)
                           , cursor   :: Maybe (V4 Float)
                           , mapMode  :: MapMode
                           }

data MapMode = RawMapMode