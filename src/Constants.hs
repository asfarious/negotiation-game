module Constants where

displayWidth :: Int
displayHeight :: Int

displayWidth = 1280
displayHeight = 960

zoomMinBound :: Float
zoomMinBound = 0.55

zoomSpeed :: Double
zoomSpeed = 0.1

cameraSpeed :: Float
cameraSpeed = 0.1

mouseScrollSensitivity :: Double
mouseScrollSensitivity = 1.0

keyScrollSensitivity :: Double
keyScrollSensitivity = 0.3

mapPath :: FilePath
mapPath = "data\\map.png"

mapHeight :: Num a => a
mapHeight = 1024

mapWidth :: Num a => a
mapWidth = 1024

mapQuadWidth :: Float
mapQuadWidth = 10.0

mapQuadHeight :: Float
mapQuadHeight = 10.0