module Tetris.Game
( initialState
, moveRight
, moveLeft
, rotateClockwise
, rotateCounterclockwise
, moveDown
, tickDown
, fullDrop
, swapSaved
, pause
, tickTime
) where

import Control.Concurrent.Suspend (Delay, msDelay)
import Control.Exception          (assert)
import Data.Function              ((&))
import Data.Maybe                 (isNothing)
import System.Random.Shuffle      (shuffle')
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

moveDown                :: AppState -> AppState
moveDown                = pausable $ (incrScore 1) . moveDown'

tickDown                :: AppState -> AppState
tickDown                = pausable $ moveDown'

fullDrop                :: AppState -> AppState
fullDrop                = pausable $ putInGrid .
                            (until isAtBottom $ shiftDown . (incrScore 2))

swapSaved               :: AppState -> AppState
swapSaved               = pausable $ addBagIfNecessary . swapSaved'

pause                   :: AppState -> AppState
pause state             = state { paused = not $ paused state }


-- Specific
pausable :: (AppState -> AppState) -> AppState -> AppState
pausable transform state = if paused state then state else transform state


swapSaved'   :: AppState -> AppState
swapSaved' state@(AppState { canSave = False }) = state
swapSaved' state@(AppState { saved
                           , next = n:nxs
                           , falling = Falling { kind }
                           }) =
    state { saved = Just kind, canSave = False } &
        case saved of
            Nothing -> \s -> s { next = nxs, falling = makeFalling n }
            Just saved' -> \s -> s { falling = makeFalling saved' }

swapSaved' AppState { next = [] } =
    error "BUG: tetramino bag should never be empty"


moveDown' :: AppState -> AppState
moveDown' state = if isAtBottom state then putInGrid state else shiftDown state


clearFullRows :: AppState -> AppState
clearFullRows state = (incrScore earnedPoints) $ state { grid = newGrid }
  where
    notFull row = any isNothing row
    cleared = filter notFull $ grid state
    numCleared = 20 - length cleared
    newGrid = cleared ++ (replicate numCleared $ replicate 10 Nothing)
    earnedPoints = case numCleared of
                     0 -> 0
                     1 -> 100
                     2 -> 300
                     3 -> 500
                     4 -> 8
                     _ -> error "BUG: Invalid number of rows cleared"


putInGrid :: AppState -> AppState
putInGrid state@(AppState { next, falling, grid, gen }) =
    if falling `fits` grid  && nextFalling `fits` nextGrid
    then withPlaced
    else newGame
  where
    withPlaced = addBagIfNecessary $ clearFullRows $ state {
                canSave = True
              , next = tail next
              , falling = nextFalling
              , grid = nextGrid
              }
    nextFalling = makeFalling $ head next
    nextGrid = placeDown falling grid
    newGame = initialState $ gen


addBagIfNecessary :: AppState -> AppState
addBagIfNecessary state@(AppState { next, gen }) =
    if length next >= 3 then state else
        state { next = next ++ bag, gen = gen' }
  where
    (gen', bag) = getBag gen


-- Small utilities
makeFalling :: Tetromino -> Falling
makeFalling kind = Falling {
    kind
,   orientation = Or0
,   pos = (18, 4)
}


incrScore :: Integer -> AppState -> AppState
incrScore points state = state { score = points + score state }


getBag :: TF.TFGen -> (TF.TFGen, [Tetromino])
getBag gen = (next, bag)
  where
    bag = shuffle' [I, O, T, J, L, S, Z] 7 gen
    (_, next) = TF.next gen

tickTime :: AppState -> Delay
tickTime AppState { score } = msDelay $ floor $ (1000.0 * (assert (time > 0) time))
  where
    time :: Double
    time = (0.8 - 0.007 * (fromInteger l)) ^ l
    l = (levelGivenScore score) - 1


-- Movement
shiftDown :: AppState -> AppState
shiftDown state = state { falling = shift (-1, 0) $ falling state }


isAtBottom :: AppState -> Bool
isAtBottom AppState { falling, grid } =
    not $ (shift (-1, 0) falling) `fits` grid


fits :: Falling -> [[CellState]] -> Bool
fits falling grid = all (\(r, c) ->
        and [0 <= r, r < 20, 0 <= c, c < 10, isNothing $ grid !! r !! c]
    ) $ cellsOccupiedBy falling


ifFits :: (Falling -> Falling) -> AppState -> AppState
ifFits transform state =
    if transformed `fits` (grid state) then
        state { falling = transformed } else state
  where
    transformed = transform $ falling state


shift :: (Int, Int) -> Falling -> Falling
shift offset falling@(Falling { pos = (r, c) }) =
    falling { pos = (r + fst offset, c + snd offset) }


rotate :: Orientation -> Falling -> Falling
rotate Or0 fall = fall
rotate orient fall = rotate (pred orient) $
    if orientation fall == Or3 then
        fall { orientation = Or0 }
    else
        fall { orientation = succ $ orientation fall }
