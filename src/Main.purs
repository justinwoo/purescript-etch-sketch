module Main where

import Prelude (Unit, bind, ($), (-), (*), (>), (||), (<), (+), (==), (&&), class Eq)
import Control.Monad.Eff (Eff)
import Data.Array as Array
import DOM (DOM)
import Data.Maybe (Maybe(Nothing, Just))
import Signal (constant, sampleOn, mergeMany, foldp, runSignal, (<~))
import Signal.DOM (keyPressed)

data Direction
  = Up
  | Down
  | Left
  | Right

data Coords = Coords Int Int
instance eqCoords :: Eq Coords where
  eq (Coords ax ay) (Coords bx by) = ax == bx && ay == by

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

isValidPoint :: State -> Coords -> Boolean
isValidPoint state coords =
  case coords of
    Coords x y ->
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
      if isValidPoint state cursor'
          then state
          else state {cursor = cursor', points = points'}

update :: Direction -> State -> State
update direction state =
  moveCursor direction state

foreign import jsRender :: forall e. State -> Eff (dom :: DOM | e) Unit
foreign import jsRenderError :: Eff (dom :: DOM) Unit

main :: Eff (dom :: DOM) Unit
main = do
  upInput <- keyPressed 38
  downInput <- keyPressed 40
  leftInput <- keyPressed 37
  rightInput <- keyPressed 39
  let directionInput =
      mergeMany
        [ sampleOn upInput $ constant Up
        , sampleOn downInput $ constant Down
        , sampleOn leftInput $ constant Left
        , sampleOn rightInput $ constant Right
        ]
  case directionInput of
    Just signal -> runSignal $ jsRender <~ foldp update initState signal
    Nothing -> jsRenderError
