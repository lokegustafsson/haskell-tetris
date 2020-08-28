import Brick.BChan
import Brick.Types
import Brick.Main
import Control.Concurrent.Timer             (TimerIO, oneShotTimer)
import Control.Monad.IO.Class               (liftIO)
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events            (Event(EvKey));
import System.Random.TF.Init                (newTFGen)
import Tetris.Draw
import Tetris.Game
import Tetris.Base                          (AppState)

type GameEvent = BrickEvent () Tick
data Tick = Tick

-- Main
main :: IO AppState
main = do
    tickChannel <- newBChan 2
    initialVty  <- buildVty
    gen         <- newTFGen
    _           <- writeBChan tickChannel Tick
    customMain initialVty buildVty (Just tickChannel) (app tickChannel) (initialState gen)
  where
    buildVty = Vty.mkVty Vty.defaultConfig

app :: BChan Tick -> App AppState Tick ()
app tickChannel = App {
  appDraw           = drawAll
, appChooseCursor   = neverShowCursor
, appHandleEvent    = makeEventHandler tickChannel
, appStartEvent     = return
, appAttrMap        = attributeMap
}


-- Handle events
makeEventHandler :: BChan Tick -> AppState -> GameEvent -> EventM () (Next AppState)
makeEventHandler tickChannel = handleEvent
  where
    handleEvent state (AppEvent Tick) = do
        _ <- liftIO $ nextTickTimer tickChannel state
        continue $ tickDown state

    handleEvent state (VtyEvent (EvKey key _)) = handleKeyEvent state key

    handleEvent state _ = continue state


handleKeyEvent :: AppState -> Vty.Key -> EventM () (Next AppState)

handleKeyEvent state (Vty.KChar 'q') = halt state

handleKeyEvent state key = continue $ transform state
  where
    transform = case key of
        Vty.KRight      -> moveRight
        Vty.KLeft       -> moveLeft
        Vty.KUp         -> rotateClockwise
        Vty.KChar 'z'   -> rotateCounterclockwise
        Vty.KDown       -> moveDown
        Vty.KChar ' '   -> fullDrop
        Vty.KChar 'c'   -> swapSaved
        Vty.KEsc        -> pause
        _               -> id

nextTickTimer :: BChan Tick -> AppState -> IO TimerIO
nextTickTimer chan state = oneShotTimer (writeBChan chan Tick) (tickTime state)
