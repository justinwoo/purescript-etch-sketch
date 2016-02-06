module Main where

import Prelude (Unit, bind, (-), (*), (>), (||), (<), (+))
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
  | Nil

type Coords =
  { x :: Int
  , y :: Int
  }

type State =
  { cursor :: Coords
  , points :: Array Coords
  , width :: Int
  , height :: Int
  , increment :: Int
  }

initState :: State
initState =
  { cursor:
      { x: 0
      , y: 0
      }
  , points:
      []
  , width: 800
  , height: 600
  , increment: 10
  }

moveCursor :: Direction -> State -> State
moveCursor direction state = do
  let x = state.cursor.x
  let y = state.cursor.y
  let points' = Array.cons state.cursor state.points
  let cursor' =
    case direction of
        Nil -> state.cursor
        Up -> {x: x, y: y - 1}
        Down -> {x: x, y: y + 1}
        Left -> {x: x - 1, y: y}
        Right -> {x: x + 1, y: y}
  if (cursor'.x < 0 || (state.increment * cursor'.x) > (state.width - state.increment) ||
      cursor'.y < 0 || (state.increment * cursor'.y) > (state.height - state.increment))
      then state
      else (state
            { cursor = cursor'
            , points = points'
            })

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
      (mergeMany
        [ sampleOn upInput (constant Up)
        , sampleOn downInput (constant Down)
        , sampleOn leftInput (constant Left)
        , sampleOn rightInput (constant Right)
        ])
  let render =
    case directionInput of
        Just signal -> do
          jsRender <~ (foldp update initState signal)
        Nothing -> constant jsRenderError
  runSignal render
