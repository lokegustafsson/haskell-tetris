module Tetris.Base
( AppState (..)
, Falling (..)
, Row
, CellState
, Tetramino (..)
, Orientation (..)
, cellsOccupiedBy
) where

import System.Random.TF

data AppState = AppState {
    paused :: Bool
,   score :: Integer
,   saved :: Maybe Tetramino
,   canSave :: Bool
,   next :: [Tetramino]
,   falling :: Falling
,   grid :: [Row]
,   gen :: TFGen
}

data Falling = Falling {
    kind :: Tetramino
,   orientation :: Orientation
,   pos :: (Int, Int)
}

type Row = [CellState]
type CellState = Maybe Tetramino

data Tetramino = I | O | T | J | L | S | Z deriving (Show)
data Orientation = Or0 | Or1 | Or2 | Or3 deriving (Enum)

cellsOccupiedBy :: Falling -> [(Int, Int)]
cellsOccupiedBy Falling { kind, orientation = o, pos } =
    map (\(r, c) -> (r + fst pos, c + snd pos)) $ case kind of
    I -> rotateCorner o [(0, -1), (0,  0), (0, 1), (0, 2)]
    O ->                [(0,  0), (1,  0), (0, 1), (1, 1)]
    T -> rotateOrigin o [(0, -1), (0,  0), (1, 0), (0, 1)]
    J -> rotateOrigin o [(0, -1), (1, -1), (0, 0), (0, 1)]
    L -> rotateOrigin o [(0, -1), (0,  0), (0, 1), (1, 1)]
    S -> rotateOrigin o [(0, -1), (0,  0), (1, 0), (1, 1)]
    Z -> rotateOrigin o [(1, -1), (0,  0), (1, 0), (0, 1)]

rotateOrigin :: Orientation -> [(Int, Int)] -> [(Int, Int)]
rotateOrigin Or0 = id
rotateOrigin orient = rotateOrigin (pred orient) .
    map (\(r, c) -> (-c, r))

rotateCorner :: Orientation -> [(Int, Int)] -> [(Int, Int)]
rotateCorner Or0 = id
rotateCorner orient = rotateCorner (pred orient) .
    map (\(r, c) -> (1-c, r))
