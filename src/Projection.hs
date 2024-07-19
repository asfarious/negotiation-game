module Projection where

import                         Graphics.GPipe

viewBoard :: Floating a => V3 a -> a -> V4 a -> V4 a
viewBoard at zoom = ((projMat !*! lookMat) !*)
    where 
        projMat = perspective (pi/3) 1 1 100
        lookMat = lookAt (at + zoom *^ (V3 0 1 1)) at (V3 0 1 0)

projectToBoard :: Floating a => V3 a -> a -> V4 a -> V4 a
projectToBoard at zoom p = point cameraPosition + (negate yCam / rayY) *^ rayDirection
    where
        cameraPosition@(V3 _ yCam _) = at + zoom *^ (V3 0 1 1)
        invProjMat = inv44 $ perspective (pi/3) 1 1 100
        invLookMat = inv44 $ lookAt cameraPosition at (V3 0 1 0)
        (V4 xView yView _ _) = invProjMat !* p
        rayDirection@(V4 _ rayY _ _) = invLookMat !* (V4 xView yView (-1) 0)
        
projectCursor :: Floating a => V3 a -> a -> V2 a -> V4 a
projectCursor at zoom (V2 x y) = projectToBoard at zoom (V4 x y (-1) 1)