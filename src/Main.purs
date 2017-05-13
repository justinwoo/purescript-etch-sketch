module Main where

import CSS (border)
import CSS.Border (solid)
import CSS.Color (black)
import CSS.Size (px)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Array (fromFoldable)
import Data.Foldable (fold)
import Data.Monoid (mempty)
import Data.Set (Set, insert)
import Pux (EffModel, start, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key, style)
import Pux.Renderer.React (renderToDOM)
import Signal (Signal)
import Signal.Channel (CHANNEL)
import Signal.DOM (keyPressed)
import Text.Smolder.HTML (button, div)
import Text.Smolder.HTML.Attributes (height, width)
import Text.Smolder.Markup (attribute, leaf, parent, text, (!), (#!))
import Prelude hiding (div)

data Direction
  = Up
  | Down
  | Left
  | Right

data Coords = Coords Int Int
derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords

data Event
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

foldp :: forall eff. Event -> State -> EffModel State Event eff
foldp (MoveCursor direction) state =
  noEffects $ moveCursor direction state
foldp ClearScreen state =
  noEffects $ state {points = mempty :: Set Coords}
foldp NoOp state =
  noEffects $ state

pointView :: Int -> String -> Coords -> HTML Event
pointView increment color (Coords x y) =
  leaf "rect"
    ! key (show x <> "," <> show y)
    ! width (show increment)
    ! height (show increment)
    ! attribute "fill" color
    ! attribute "x" (show (x * increment))
    ! attribute "y" (show (y * increment))

view :: State -> HTML Event
view state =
  div do
    div ! key "buttondiv" $ do
      button
        #! onClick (const ClearScreen)
        $ do
          text "Clear"
    div ! key "svgdiv" $ do
      parent "svg"
        ! style (border solid (px 1.0) black)
        ! width (show state.width)
        ! height (show state.height)
        $ do
          fold points
          cursor
    where
      pointView' = pointView state.increment
      points = pointView' "black" <$> fromFoldable state.points
      cursor = pointView' "grey" state.cursor

getKeyDirections :: forall eff.
  Eff
    ( dom :: DOM
    | eff
    )
    (Signal Event)
getKeyDirections = do
  ups <- map (event $ MoveCursor Up) <$> keyPressed 38
  downs <- map (event $ MoveCursor Down) <$> keyPressed 40
  lefts <- map (event $ MoveCursor Left) <$> keyPressed 37
  rights <- map (event $ MoveCursor Right) <$> keyPressed 39
  pure $ ups <> downs <> lefts <> rights
  where
    event x = if _ then x else NoOp

main :: forall eff.
  Eff
    ( dom :: DOM
    , channel :: CHANNEL
    , exception :: EXCEPTION
    | eff
    )
    Unit
main = do
  keyDirections <- getKeyDirections
  app <- start
    { initialState
    , foldp
    , view
    , inputs:
      [ keyDirections
      ]
    }

  renderToDOM "#app" app.markup app.input
