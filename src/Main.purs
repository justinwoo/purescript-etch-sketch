module Main where

import Prelude hiding (div)
import CSS (border)
import CSS.Border (solid)
import CSS.Color (black)
import CSS.Size (px)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Array ((:))
import Data.Int (toNumber)
import Pux (start, fromSimple, renderToDOM)
import Pux.CSS (style)
import Pux.Html (Html, div, text, button, svg, rect)
import Pux.Html.Attributes (height, width, key)
import Pux.Html.Events (onClick)
import Signal (Signal, constant, (<~))
import Signal.Channel (CHANNEL)

foreign import data CrappyHashSet :: * -> *
foreign import empty :: forall a. CrappyHashSet a
foreign import insert :: forall a. (CrappyHash a) => a -> CrappyHashSet a -> CrappyHashSet a
foreign import mapToArray :: forall a b. (a -> b) -> CrappyHashSet a -> Array b

class CrappyHash a where
  crappyHash :: a -> String

data Direction
  = Up
  | Down
  | Left
  | Right

data Coords = Coords Int Int
derive instance eqCoords :: Eq Coords
-- instance eqCoords :: Eq Coords where
--   eq (Coords ax ay) (Coords bx by) = ax == bx && ay == by
instance crappyHashCoords :: CrappyHash Coords where
  crappyHash (Coords x y) = "x:" ++ (show x) ++ "y:" ++ (show y)

data Action
  = MoveCursor Direction
  | ClearScreen
  | NoOp

type State =
  { cursor :: Coords
  , points :: CrappyHashSet Coords
  , width :: Int
  , height :: Int
  , increment :: Int
  }

initialState :: State
initialState =
  { cursor: Coords 0 0
  , points: empty
  , width: 800
  , height: 600
  , increment: 10
  }

isInvalidPoint :: State -> Coords -> Boolean
isInvalidPoint state (Coords x y) =
  x < 0 || (state.increment * x) > (state.width - state.increment) ||
  y < 0 || (state.increment * y) > (state.height - state.increment)

insertPoint :: Coords -> CrappyHashSet Coords -> CrappyHashSet Coords
insertPoint point points =
  insert point points

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

update :: Action -> State -> State
update (MoveCursor direction) state =
  moveCursor direction state
update ClearScreen state =
  state { points = empty }
update NoOp state =
  state

pointView :: Int -> String -> Coords -> Html Action
pointView increment subkey (Coords x y) =
  rect
    [ key (subkey ++ show x ++ "," ++ show y)
    , width (show increment)
    , height (show increment)
    , (Pux.Html.Attributes.x (show $ x * increment))
    , (Pux.Html.Attributes.y (show $ y * increment))
    ]
    []

view :: State -> Html Action
view state =
  let
    pointView' = pointView state.increment
    cursor = pointView' "cursor" state.cursor
    points = mapToArray (pointView' "pointView") state.points
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

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL, dom :: DOM | e) Unit
main = do
  keydown' <- keydown
  app <- start
    { initialState: initialState
    , update: fromSimple update
    , view: view
    , inputs:
      [ keyDirections <~ keydown'
      ]
    }

  renderToDOM "#app" app.html
