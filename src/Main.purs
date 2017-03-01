module Main where

import Pux.Html.Attributes as HA
import CSS (border)
import CSS.Border (solid)
import CSS.Color (black)
import CSS.Size (px)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Array (fromFoldable, (:))
import Data.Int (toNumber)
import Data.Monoid (mempty)
import Data.Set (Set, insert)
import Pux (start, fromSimple, renderToDOM)
import Pux.CSS (style)
import Pux.Html (Html, div, text, button, svg, rect)
import Pux.Html.Attributes (height, key, width)
import Pux.Html.Events (onClick)
import Signal (Signal)
import Signal.Channel (CHANNEL)
import Signal.DOM (keyPressed)
import Prelude hiding (div)

data Direction
  = Up
  | Down
  | Left
  | Right

data Coords = Coords Int Int
derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords

data Action
  = MoveCursor Direction
  | ClearScreen
  | NoOp

type State =
  { cursor :: Coords
  , points :: Set Coords
  , width :: Int
  , height :: Int
  , increment :: Int
  }

initialState :: State
initialState =
  { cursor: Coords 0 0
  , points: mempty
  , width: 800
  , height: 600
  , increment: 10
  }

isInvalidPoint :: State -> Coords -> Boolean
isInvalidPoint {increment, width, height} (Coords x y)
  | x < 0 = true
  | y < 0 = true
  | x > width / increment - 1 = true
  | y > height / increment - 1 = true
  | otherwise = false

moveCursor :: Direction -> State -> State
moveCursor direction state@{cursor: (Coords x y)} =
  if isInvalidPoint state cursor'
    then state
    else state {cursor = cursor', points = points'}
  where
    points' = insert state.cursor state.points
    cursor' = case direction of
      Up -> Coords x (y - 1)
      Down -> Coords x (y + 1)
      Left -> Coords (x - 1) y
      Right -> Coords (x + 1) y

update :: Action -> State -> State
update (MoveCursor direction) state =
  moveCursor direction state
update ClearScreen state =
  state { points = mempty }
update NoOp state =
  state

pointView :: Int -> String -> Coords -> Html Action
pointView increment subkey (Coords x y) =
  rect
    [ key (subkey <> show x <> "x" <> show y <> "y")
    , width (show increment)
    , height (show increment)
    , (HA.x (show $ x * increment))
    , (HA.y (show $ y * increment))
    ]
    []

view :: State -> Html Action
view state =
  let
    pointView' = pointView state.increment
    cursor = pointView' "cursor" state.cursor
    points = pointView' "pointView" <$> fromFoldable state.points
  in
    div
      []
      [ div
        []
        [ button
          [ onClick (const ClearScreen) ]
          [ text "Clear" ]
        ]
      , div
        []
        [ svg
          [ style $ do
            border solid (px (toNumber 1)) black
          , width (show state.width)
          , height (show state.height)
          ]
          $ cursor : points
        ]
      ]

getKeyDirections :: forall e. Eff (dom :: DOM | e) (Signal Action)
getKeyDirections = do
  ups <- map (actions $ MoveCursor Up) <$> keyPressed 38
  downs <- map (actions $ MoveCursor Down) <$> keyPressed 40
  lefts <- map (actions $ MoveCursor Left) <$> keyPressed 37
  rights <- map (actions $ MoveCursor Right) <$> keyPressed 39
  pure $ ups <> downs <> lefts <> rights
  where
    actions x = if _ then x else NoOp

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL, dom :: DOM | e) Unit
main = do
  keyDirections <- getKeyDirections
  app <- start
    { initialState
    , update: fromSimple update
    , view
    , inputs:
      [ keyDirections
      ]
    }

  renderToDOM "#app" app.html
