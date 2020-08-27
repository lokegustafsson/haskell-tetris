module Tetris.Game
( initialState
, moveRight
, moveLeft
, rotateClockwise
, rotateCounterclockwise
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
moveRight               :: AppState -> AppState
moveRight               = pausable $ ifFits $ shift (0, 1)

moveLeft                :: AppState -> AppState
moveLeft                = pausable $ ifFits $ shift (0, -1)

rotateClockwise         :: AppState -> AppState
rotateClockwise         = pausable $ ifFits $ rotate Or1

rotateCounterclockwise  :: AppState -> AppState
rotateCounterclockwise  = pausable $ ifFits $ rotate Or3

tickDown                :: AppState -> AppState
tickDown                = pausable $ tickDown'

fullDrop                :: AppState -> AppState
fullDrop                = pausable $ until isAtBottom moveDown

swapSaved               :: AppState -> AppState
swapSaved               = pausable $ addBagIfNecessary . swapSaved'

pause                   :: AppState -> AppState
pause state             = state { paused = not $ paused state }


-- Util
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

tickDown' :: AppState -> AppState
tickDown' state@(AppState { next, falling, grid }) =
    if shifted `fits` grid then
        incrScore $ state { falling = shifted }
    else
        addBagIfNecessary $ state {
                canSave = True
              , next = tail next
              , falling = makeFalling $ head next
              , grid = placeDown falling grid
              }
  where
    shifted = shift (-1, 0) falling

moveDown :: AppState -> AppState
moveDown state@(AppState { score, falling }) =
    state { score = score + 1, falling = shift (-1, 0) falling }

isAtBottom :: AppState -> Bool
isAtBottom AppState { falling, grid } =
    not $ (shift (-1, 0) falling) `fits` grid


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

-- Movement
fits :: Falling -> [[CellState]] -> Bool
fits falling grid = all (\(r, c) ->
        and [0 <= r, r < 20, 0 <= c, c < 10, isNothing $ grid !! r !! c]
    ) $ cellsOccupiedBy falling

shift :: (Int, Int) -> Falling -> Falling
shift offset falling@(Falling { pos = (r, c) }) =
    falling { pos = (r + fst offset, c + snd offset) }

ifFits :: (Falling -> Falling) -> AppState -> AppState
ifFits transform state =
    if transformed `fits` (grid state) then
        state { falling = transformed } else state
  where
    transformed = transform $ falling state

rotate :: Orientation -> Falling -> Falling
rotate Or0 fall = fall
rotate orient fall = rotate (pred orient) $
    if orientation fall == Or3 then
        fall { orientation = Or0 }
    else
        fall { orientation = succ $ orientation fall }
