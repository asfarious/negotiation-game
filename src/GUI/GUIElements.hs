module GUI.GUIElements where

import                         Graphics.GPipe
import                         Text.Printf                          (printf)
--
import                         GUI.GUIState
import                         Events


cursorStatusPreElement :: PreGUIElement Event
cursorStatusPreElement = ( V4 0 0 555 90
                         , (\_ _ -> [])
                         , \bBox@(V4 bbX bbY _ _) (cur2D, cur3D) 
                              -> let cur2DText = case cur2D of
                                                      Left (V2 x y)  -> "2D Cursor: " ++ printf "%d, %d" x y
                                                      Right (V2 x y) -> "2D Cursor: " ++ printf "%.2f, %.2f" x y
                                     cur3DText = case cur3D of
                                                      Just (V3 x y z) -> "3D Cursor: " ++ printf "%.2f, %.2f, %.2f" x y z
                                                      Nothing         -> "Cursor is on GUI!"
                                 in [ ColoredBox bBox (V4 1 0 0 1)
                                    , TextBox (V4 (bbX+5) (bbY+40) 1 1) cur2DText
                                    , TextBox (V4 (bbX+5) (bbY+80) 1 1) cur3DText
                                    ]
                         )