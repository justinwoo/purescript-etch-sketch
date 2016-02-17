module Main where

import Prelude (class Eq, Unit, ($), (<>), bind, show, map, (*), (++), (+), (-), (>), (||), (<), (==), (&&))
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Data.Array as Array
import Data.Foldable (mconcat)
import Data.Maybe (Maybe(Nothing, Just))
import DOM (DOM)
import Pux.App (app, Update())
import Pux.DOM (VirtualDOM(), (!))
import Pux.DOM.HTML.Elements as E
import Pux.DOM.HTML.Attributes as A
import Pux.Render.DOM (renderToDOM)
import Signal (Signal, constant, (<~))
import Signal.Channel (CHANNEL)

data Direction
  = Up
  | Down
  | Left
  | Right

data Coords = Coords Int Int
instance eqCoords :: Eq Coords where
  eq (Coords ax ay) (Coords bx by) = ax == bx && ay == by

data Action
  = MoveCursor Direction
  | ClearScreen
  | NoOp

type State =
  { cursor :: Coords
  , points :: Array Coords
  , width :: Int
  , height :: Int
  , increment :: Int
  }

initState :: State
initState =
  { cursor: Coords 0 0
  , points: []
  , width: 800
  , height: 600
  , increment: 10
  }

isInvalidPoint :: State -> Coords -> Boolean
isInvalidPoint state (Coords x y) =
  x < 0 || (state.increment * x) > (state.width - state.increment) ||
  y < 0 || (state.increment * y) > (state.height - state.increment)

insertPoint :: Coords -> Array Coords -> Array Coords
insertPoint point points =
  case Array.elemIndex point points of
    Just _ -> points
    Nothing -> Array.cons point points

moveCursor :: Direction -> State -> State
moveCursor direction state =
  case state.cursor of
    Coords x y -> do
      let points' = insertPoint state.cursor state.points
      let cursor' =
        case direction of
            Up -> Coords x (y - 1)
            Down -> Coords x (y + 1)
            Left -> Coords (x - 1) y
            Right -> Coords (x + 1) y
      if isInvalidPoint state cursor'
        then state
        else state {cursor = cursor', points = points'}

update :: forall eff. Update (eff) State Action
update (MoveCursor direction) state input =
  { state: moveCursor direction state
  , effects: []
  }
update ClearScreen state input =
  { state: state {points = []}
  , effects: []
  }
update NoOp state input =
  { state: state
  , effects: []
  }

pointView :: Int -> String -> Coords -> VirtualDOM
pointView increment subkey (Coords x y) =
  E.rect
    ! A.key (subkey ++ show x ++ "," ++ show y)
    ! A.width (show increment)
    ! A.height (show increment)
    ! A.x (show $ x * increment)
    ! A.y (show $ y * increment)

view :: State -> VirtualDOM
view state =
  let
    pointView' = pointView state.increment
    cursor = pointView' "cursor" state.cursor
    points = map (pointView' "pointView") state.points
  in
    E.div $ do
      E.div $
        E.button ! A.onClick (A.send ClearScreen) $ E.text "Clear"
      E.div $
        E.svg
          ! A.style { border: "1px solid black" }
          ! A.width (show state.width)
          ! A.height (show state.height)
          $ cursor <> mconcat points

foreign import keydownP :: forall e c. (c -> Signal c) -> Eff (dom :: DOM | e) (Signal Int)

keydown :: forall e. Eff (dom :: DOM | e) (Signal Int)
keydown = keydownP constant

keyDirections :: Int -> Action
keyDirections keyCode =
  case keyCode of
    38 -> MoveCursor Up
    40 -> MoveCursor Down
    37 -> MoveCursor Left
    39 -> MoveCursor Right
    _ -> NoOp

main :: forall e. Eff (channel :: CHANNEL, dom :: DOM | e) Unit
main = do
  keydown' <- keydown
  renderToDOM "#app" =<< app
    { state: initState
    , update: update
    , view: view
    , inputs:
        [ keyDirections <~ keydown'
        ]
    }
