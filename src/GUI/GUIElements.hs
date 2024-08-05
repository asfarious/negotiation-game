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
                                                                coloredButton (V4 455 0 100 40) (V4 0 0 1 1) $ \self _ -> [Event'GUIEvent $ DeleteElement self]

clickWindowPreElement (V2 x y) = flip newPreElement (V4 0 200 555 50) $ do
                                                                coloredBackground (V4 1 1 0 1)
                                                                textBox (V4 5 40 1 1) $ \_ -> printf "Clicked at: %d, %d" x y
                                                                onClick $ \self click -> case click of
                                                                        GUILeftClick _  -> []
                                                                        GUIRightClick _ -> [Event'GUIEvent $ DeleteElement self]