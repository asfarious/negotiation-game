module GUI.GUIElements where

import                         Graphics.GPipe
import                         Text.Printf                          (printf)
--
import                         GUI.GUIState
import                         Events
import                         GUI.DescribeGUI


cursorStatusPreElement = flip newPreElement (V4 0 0 555 90) $ do                      
                                                                coloredBackground (V4 1 0 0 1)
                                                                textBox (V4 5 40 1 1) $ \(cur2D, _) -> case cur2D of
                                                                                                  Left (V2 x y)  -> "2D Cursor: " ++ printf "%d, %d" x y
                                                                                                  Right (V2 x y) -> "2D Cursor: " ++ printf "%.2f, %.2f" x y
                                                                textBox (V4 5 80 1 1) $ \(_, cur3D) -> case cur3D of
                                                                                                  Just (V3 x y z) -> "3D Cursor: " ++ printf "%.2f, %.2f, %.2f" x y z
                                                                                                  Nothing         -> "Cursor is on GUI!"
                                                                onClick $ \_ click -> case click of
                                                                        GUIRightClick _-> []
                                                                        GUILeftClick at -> [Event'GUIEvent $ CreateElement (clickWindowPreElement at)]

clickWindowPreElement (V2 x y) = flip newPreElement (V4 0 200 555 50) $ do
                                                                coloredBackground (V4 1 1 0 1)
                                                                textBox (V4 5 40 1 1) $ \_ -> printf "Clicked at: %d, %d" x y
                                                                onClick $ \self click -> case click of
                                                                        GUILeftClick _  -> []
                                                                        GUIRightClick _ -> [Event'GUIEvent $ DeleteElement self]

{---
cursorStatusPreElement :: PreGUIElement Event
cursorStatusPreElement = ( V4 0 0 555 90
                         , \_ click -> case click of
                                            GUIRightClick _-> []
                                            GUILeftClick at -> [Event'GUIEvent $ CreateElement (clickWindowPreElement at)]
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

clickWindowPreElement :: V2 Int -> PreGUIElement Event
clickWindowPreElement (V2 x y) = ( V4 0 200 555 50
                                 , \self click -> case click of
                                                    GUILeftClick _  -> []
                                                    GUIRightClick _ -> [Event'GUIEvent $ DeleteElement self]
                                 , \bBox@(V4 bbX bbY _ _) _ ->
                                    [ ColoredBox bBox (V4 1 1 0 1)
                                    , TextBox (V4 (bbX+5) (bbY+40) 1 1) $ printf "Clicked at: %d, %d" x y
                                    ]
                                 )
--}