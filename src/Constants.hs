module Constants where

displayWidth :: Int
displayHeight :: Int

displayWidth = 1280
displayHeight = 960

logicUpdateTime :: Double
logicUpdateTime = (1/60)

zoomMinBound :: Float
zoomMinBound = 1.2

initialZoom :: Float
initialZoom = 10*zoomMinBound

zoomSpeed :: Double
zoomSpeed = 0.2

cameraSpeed :: Float
cameraSpeed = 0.1

mouseScrollSensitivity :: Double
mouseScrollSensitivity = 1.0

keyScrollSensitivity :: Double
keyScrollSensitivity = 0.3

mapPath :: FilePath
mapPath = "data\\map.png"

provinceMapPath :: FilePath
provinceMapPath = "data\\provinces.png" --

mapHeight :: Num a => a
mapHeight = 1024

mapWidth :: Num a => a
mapWidth = 1024

mapQuadWidth :: Float
mapQuadWidth = 10.0

mapQuadHeight :: Float
mapQuadHeight = 10.0

defaultFontFile :: FilePath
defaultFontFile = "data/font/NotoSans-Regular.ttf"