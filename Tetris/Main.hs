module Tetris.Main (main) where

import Prelude hiding (Left, Right)
import Brick.BChan
import Brick.Types
import Brick.Main
import Control.Concurrent.Timer             (repeatedTimer)
import Control.Concurrent.Suspend.Lifted    (msDelay)
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events            (Event(EvKey));
import System.Random.TF.Init                (newTFGen)
import Tetris.Draw
import Tetris.Game
import Tetris.Base

type GameEvent = BrickEvent () Tick
data Tick = Tick

-- MAIN
main :: IO AppState
main = do
    tickChannel <- newBChan 10
    initialVty  <- buildVty
    gen         <- newTFGen
    _           <- repeatedTimer (writeBChan tickChannel $ Tick) (msDelay 800)
    customMain initialVty buildVty (Just tickChannel) app (initialState gen)
  where
    buildVty = Vty.mkVty Vty.defaultConfig

-- App <application state type> <event type> <resource name type>
app :: App AppState Tick ()
app = App {
  appDraw           = drawAll
, appChooseCursor   = neverShowCursor
, appHandleEvent    = handleEvent
, appStartEvent     = return
, appAttrMap        = attributeMap
}


-- Handle events
handleEvent :: AppState -> GameEvent -> EventM () (Next AppState)

handleEvent state (AppEvent Tick) = continue $ tickDown state

handleEvent state (VtyEvent (EvKey (Vty.KChar 'q') _)) = halt state

handleEvent state (VtyEvent (EvKey pressedKey _)) = continue $ action state
  where
    action = case pressedKey of
        Vty.KRight      -> moveRight
        Vty.KLeft       -> moveLeft
        Vty.KUp         -> rotate
        Vty.KChar 'z'   -> rotateCC
        Vty.KDown       -> tickDown
        Vty.KChar ' '   -> fullDrop
        Vty.KChar 'c'   -> swapSaved
        Vty.KEsc        -> pause
        _               -> id

handleEvent s _ = continue s
