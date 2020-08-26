module Tetris.Game
( initialState
, moveRight
, moveLeft
, rotate
, rotateCC
, tickDown
, fullDrop
, swapSaved
, pause
) where

import Data.Function         ((&))
import Data.Maybe            (isNothing)
import System.Random.Shuffle (shuffle')
import qualified System.Random.TF.Gen as TF
import Tetris.Base

-- Init
initialState :: TF.TFGen -> AppState
initialState gen = AppState {
    paused  = False
,   score   = 0
,   saved   = Nothing
,   canSave = True
,   next    = tail bag
,   falling  = makeFalling $ head bag
,   grid = replicate 20 $ replicate 10 Nothing
,   gen = gen'
} where
    (gen', bag) = getBag gen


-- AppState transformations
moveRight   :: AppState -> AppState
moveRight = pausable $ move (0, 1)

moveLeft    :: AppState -> AppState
moveLeft = pausable $ move (0, -1)

rotate      :: AppState -> AppState
rotate = id

rotateCC    :: AppState -> AppState
rotateCC = id

tickDown    :: AppState -> AppState
tickDown = pausable $ incrScore . move (-1, 0)

fullDrop    :: AppState -> AppState
fullDrop = id

swapSaved   :: AppState -> AppState
swapSaved = pausable $ addBagIfNecessary . swapSaved'

swapSaved'   :: AppState -> AppState
swapSaved' state@(AppState { canSave = False }) = state
swapSaved' state@(AppState {
    saved
,   next = n:nxs
,   falling = Falling { kind }
}) = state { saved = Just kind, canSave = False } &
    case saved of
      Nothing -> \s -> s { next = nxs, falling = makeFalling n }
      Just saved' -> \s -> s { falling = makeFalling saved' }

swapSaved' AppState { next = [] } =
    error "BUG: tetramino bag should never be empty"

pause       :: AppState -> AppState
pause state = state { paused = not $ paused state }


-- Util
pausable :: (AppState -> AppState) -> AppState -> AppState
pausable stateTransform state =
    if paused state then state else stateTransform state

makeFalling :: Tetramino -> Falling
makeFalling kind = Falling {
    kind
,   orientation = Or0
,   pos = (18, 4)
}

incrScore :: AppState -> AppState
incrScore state = state { score = succ $ score state }

allEmpty :: [[CellState]] -> [(Int, Int)] -> Bool
allEmpty grid points = all (\(r, c) ->
        and [0 <= r, r < 20, 0 <= c, c < 10, isNothing $ grid !! r !! c]
    ) points

move  :: (Int, Int) -> AppState -> AppState
move offset state@(AppState { falling, grid }) =
    if allEmpty grid $ cellsOccupiedBy shifted then
        state { falling = shifted }
    else state
  where
    shifted = shift offset falling

shift :: (Int, Int) -> Falling -> Falling
shift offset falling@(Falling { pos = (r, c) }) =
    falling { pos = (r + fst offset, c + snd offset) }

addBagIfNecessary :: AppState -> AppState
addBagIfNecessary state@(AppState { next, gen }) =
    if length next >= 3 then state else
        state { next = next ++ bag, gen = gen' }
  where
    (gen', bag) = getBag gen

getBag :: TF.TFGen -> (TF.TFGen, [Tetramino])
getBag gen = (next, bag)
  where
    bag = shuffle' [I, O, T, J, L, S, Z] 7 gen
    (_, next) = TF.next gen
