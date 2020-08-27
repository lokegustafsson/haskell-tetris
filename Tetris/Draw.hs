module Tetris.Draw
( attributeMap
, drawAll
) where

import Prelude hiding (Left, Right)
import Brick.AttrMap
import Brick.Types                  (Widget)
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style   (unicode)
import Brick.Widgets.Core
import qualified Graphics.Vty as Vty
import Tetris.Base

-- Attributes
attributeMap :: AppState -> AttrMap
attributeMap = const $ attrMap Vty.defAttr $ minoColor ++ attrs
  where
    attrs = [
            (attrName $ show (Nothing :: CellState), bg Vty.black)
        ,   (attrName $ show (Nothing :: CellState), fg Vty.white)
        ]
    minoColor =
        map (\(mino, color) -> (attrName $ show $ Just mino, bg color))
        [   (I, Vty.brightBlue)
        ,   (O, Vty.yellow)
        ,   (T, Vty.magenta)
        ,   (J, Vty.blue)
        ,   (L, Vty.brightRed)
        ,   (S, Vty.green)
        ,   (Z, Vty.red)
        ]


-- Widgets
drawAll :: AppState -> [Widget ()]
drawAll state = (:[]) $ withBorderStyle unicode $ hBox $ map (\box ->
    border $ box state
    ) [ gameBox, infoBox ]

infoBox :: AppState -> Widget ()
infoBox state = vBox $ map str [ pauseText, scoreText, savedText, nextText ]
  where
    pauseText = if paused state then "Paused!" else " "
    scoreText = "Score: "   ++ (show $ score state)
    savedText = "Saved "    ++ (show $ saved state)
    nextText  = "Next up: " ++ (show $ next state)

gameBox :: AppState -> Widget ()
gameBox AppState { falling, grid } =
    vBox $ reverse $ map (\row ->
        hBox $ map stateToWidget row
    ) $ placeDown falling grid

stateToWidget :: CellState -> Widget ()
stateToWidget (Just t) = withAttr (attrName $ show $ Just t) $ str "    \n    "
stateToWidget state    = withAttr (attrName $ show state)    $ str " .  \n    "
