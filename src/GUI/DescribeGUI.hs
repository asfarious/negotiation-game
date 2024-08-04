module GUI.DescribeGUI where

import                         Graphics.GPipe
import                         Control.Monad.Trans.Writer.CPS
import                         Data.Foldable                    (foldMap')
--
import                         GUI.GUIState
import                         Events

type DescribeGUI = Writer ([GUIEventHandler Event], [GUIRenderHandler]) --

newPreElement :: DescribeGUI () -> BoundingBox -> PreGUIElement Event
newPreElement description bBox = case execWriter description of
                                    (eventHandlers, renderHandlers) -> ( bBox
                                                                       , \x y -> foldMap' (\f -> f x y) eventHandlers
                                                                       , \x y -> foldMap' (\f -> f x y) renderHandlers
                                                                       )


guiBox :: (BoundingBox -> (Either (V2 Int) (V2 Float), Maybe (V3 Float)) -> GUIBox) -> DescribeGUI ()
guiBox renderHandler = tell ([\_ _ -> []], [\x y -> [renderHandler x y]])

onClick :: GUIEventHandler Event -> DescribeGUI ()
onClick eventHandler = tell ([eventHandler], [\_ _ -> []])

coloredBackground :: V4 Float -> DescribeGUI ()
coloredBackground color = guiBox $ \bBox _ -> ColoredBox bBox color

-- texturedBackground :: ?? -> DescribeGUI ()

coloredBox :: V4 Int -> V4 Float -> DescribeGUI ()
coloredBox (V4 x y width height) color = guiBox $ \(V4 boxX boxY _ _) _ -> ColoredBox (V4 (boxX + x) (boxY + y) width height) color

-- texturedBox

textBox :: V4 Int -> ((Either (V2 Int) (V2 Float), Maybe (V3 Float)) -> String) -> DescribeGUI ()
textBox at text = guiBox $ \bBox@(V4 x y _ _) state -> TextBox (V4 x y 0 0 + at) $ text state

coloredButton :: V4 Int -> V4 Float -> GUIEventHandler Event -> DescribeGUI ()
coloredButton buttonBox@(V4 x y width height) color eventHandler = tell ( [\elemId click -> if isInBoundingBox (getCursor click) buttonBox
                                                                            then eventHandler elemId click
                                                                            else []
                                                                          ]
                                                                        , [\(V4 boxX boxY _ _) _ -> [ColoredBox (V4 (boxX + x) (boxY + y) width height) color]]
                                                                        ) 