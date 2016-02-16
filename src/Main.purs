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
import Pux.React (makeAttr)
import Pux.Render.DOM (renderToDOM)
import Signal (sampleOn, constant)
import Signal.Channel (CHANNEL)
import Signal.DOM (keyPressed)

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

pointView :: Int -> String -> Coords -> VirtualDOM
pointView increment subkey (Coords x y) =
  E.leaf "rect"
    ! A.key (subkey ++ show x ++ "," ++ show y)
    ! A.width (show increment)
    ! A.height (show increment)
    ! makeAttr "x" (show $ x * increment)
    ! makeAttr "y" (show $ y * increment)

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
        E.parent "svg"
          ! A.style { border: "1px solid black" }
          ! A.width (show state.width)
          ! A.height (show state.height)
          $ cursor <> mconcat points

main :: forall e. Eff (channel :: CHANNEL, dom :: DOM | e) Unit
main = do
  upInput <- keyPressed 38
  downInput <- keyPressed 40
  leftInput <- keyPressed 37
  rightInput <- keyPressed 39
  renderToDOM "#app" =<< app
    { state: initState
    , update: update
    , view: view
    , inputs:
        [ sampleOn upInput $ constant $ MoveCursor Up
        , sampleOn downInput $ constant $ MoveCursor Down
        , sampleOn leftInput $ constant $ MoveCursor Left
        , sampleOn rightInput $ constant $ MoveCursor Right
        ]
    }
