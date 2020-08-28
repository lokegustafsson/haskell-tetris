module Tetris.Draw
( attributeMap
, drawAll
) where

import Brick.AttrMap
import Brick.Types                  (Widget)
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style   (borderStyleFromChar, unicode)
import Brick.Widgets.Center
import Brick.Widgets.Core
import Data.List                    (intersperse)
import Data.Maybe                   (isNothing)
import qualified Graphics.Vty as Vty
import Tetris.Base

-- Attributes
attributeMap :: AppState -> AttrMap
attributeMap = const $ attrMap Vty.defAttr $ minoColor
  where
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


-- Draw all
drawAll :: AppState -> [Widget ()]
drawAll state = (:[]) $ center $
    spaceBorder $
    unicodeBorderWithLabel "TETRIS" $
    spaceBorder $
    withBorderStyle unicode $ vLimit 80 $ hLimit 62 $
    stb <=> (gb <+> hLimit 20 (nb <=> svb <=> helpBox <=> scoreBox))
  where
    spaceBorder = withBorderStyle (borderStyleFromChar ' ') . border
    unicodeBorderWithLabel label =
        withBorderStyle unicode . (borderWithLabel $ str label)
    gb = gameBox state
    stb = statsBox state
    nb = nextBox state
    svb = saveBox state

-- Widgets
gameBox :: AppState -> Widget ()
gameBox AppState { paused, falling, grid } =
    borderWithLabel borderText $ vBox $ reverse $ map (\row ->
        hBox $ map cellWidget row
    ) $ placeDown falling grid
  where
    borderText = str $ if paused then "Paused!" else "Playing!"

statsBox :: AppState -> Widget ()
statsBox AppState { score } = borderWithLabel (str "Stats") $ hBox $ map (hCenter . str)
        ["Score: " ++ show score, "Level: " ++ show level]
  where
    level = levelGivenScore score

nextBox :: AppState -> Widget ()
nextBox AppState { next } =
    borderWithLabel (str "Next up") $ vBox $ intersperse space $
    map singleTetromino $ take 3 next

saveBox :: AppState -> Widget ()
saveBox AppState { saved } = borderWithLabel (str "Saved") $ (\w -> space <=> w <=> space) $
    case saved of
      Just t -> singleTetromino t
      Nothing -> inTetrominoSpace $ str " "


helpBox :: Widget ()
helpBox = drawTable "Help" [(     "Quit",     "q")
                           ,(    "Pause",   "esc")
                           ,(    "Right",     "→")
                           ,(     "Left",     "←")
                           ,(   "Rotate",  "z, ↑")
                           ,(     "Hold",     "c")
                           ,("Soft drop",     "↓")
                           ,("Hard drop", "space")]

scoreBox :: Widget ()
scoreBox = drawTable "Scoring" [(  "Line x1",    "100")
                               ,(  "Line x2",    "300")
                               ,(  "Line x3",    "500")
                               ,(  "Line x4",    "800")
                               ,("Soft drop", "1/tile")
                               ,("Hard drop", "2/tile")]

-- Helpers
drawTable :: String -> [(String, String)] -> Widget ()
drawTable title table = borderWithLabel (str title) $ vBox $
    map (\(l, r) -> (str l) <+> (hCenter $ str " ") <+> (str r)) table

singleTetromino :: Tetromino -> Widget ()
singleTetromino kind = inTetrominoSpace . vBox . reverse $ map rowWidget rows
  where
    rowWidget r = hBox $ map (\c -> widgetAtCell (r, c)) $ columns
    widgetAtCell pos = (`applyColor` emptyCell) $
        if pos `elem` occupied then Just kind else Nothing
    occupied = cellsOccupiedBy Falling { kind, orientation = Or0, pos = (0, 0) }
    rows = if kind == I then [0] else [0, 1]
    columns = case kind of I -> [-1..2]
                           O -> [0, 1]
                           _ -> [-1..1]

cellWidget :: CellState -> Widget ()
cellWidget state = applyColor state $ if isNothing state then dottedCell
                                                            else emptyCell
applyColor :: CellState -> Widget () -> Widget ()
applyColor state = withAttr $ attrName $ show state

inTetrominoSpace :: Widget () -> Widget ()
inTetrominoSpace = (vLimit 4) . center

space :: Widget ()
space = str " "

emptyCell :: Widget ()
emptyCell = str "    \n    "

dottedCell :: Widget ()
dottedCell = str " .  \n    "
