import Char exposing (KeyCode)
import Html exposing (..)
import Html.Attributes as Attrs
import Keyboard
import Json.Encode as Json
import Svg
import Svg.Attributes as SvgAttrs

type Direction
  = Up
  | Down
  | Left
  | Right

type Coords = Coords Int Int

type alias State =
  { cursor : Coords
  , points : List Coords
  , width : Int
  , height : Int
  , increment : Int
  }

initState : State
initState =
  { cursor = Coords 0 0
  , points = []
  , width = 800
  , height = 600
  , increment = 10
  }

isValidPoint : State -> Coords -> Bool
isValidPoint state coords =
  case coords of
    Coords x y ->
      x < 0 || (state.increment * x) > (state.width - state.increment) ||
      y < 0 || (state.increment * y) > (state.height - state.increment)

insertPoint : Coords -> List Coords -> List Coords
insertPoint point points =
  case List.member point points of
    True -> points
    False -> point :: points

moveCursor : Direction -> State -> State
moveCursor direction state =
  case state.cursor of
    Coords x y ->
      let
        points' = insertPoint state.cursor state.points
        cursor' =
          case direction of
              Up -> Coords x (y - 1)
              Down -> Coords x (y + 1)
              Left -> Coords (x - 1) y
              Right -> Coords (x + 1) y
      in
        if isValidPoint state cursor'
          then state
          else {state | cursor = cursor', points = points'}

point : Int -> String -> Coords -> Html
point increment subkey (Coords x y) =
  Svg.rect
    [ Attrs.key <| subkey ++ toString x ++ "," ++ toString y
    , SvgAttrs.width <| toString increment
    , SvgAttrs.height <| toString increment
    , SvgAttrs.x <| toString <| x * increment
    , SvgAttrs.y <| toString <| y * increment
    ]
    []

view : State -> Html
view state =
  let
    point' = point state.increment
    cursor = point' "cursor" state.cursor
    points = List.map (point' "point") state.points
  in
    div
      [ Attrs.style
        [ ("border", "1px solid black")
        , ("width", (toString state.width) ++ "px")
        , ("height", (toString state.height) ++ "px")
        ]
      ]
      [ Svg.svg
          [ SvgAttrs.width <| (toString state.width)
          , SvgAttrs.height <| (toString state.height)
          ]
          <| cursor :: points
      ]

update : Direction -> State -> State
update direction state =
  moveCursor direction state

getKeyDirection : KeyCode -> Maybe Direction
getKeyDirection keyCode =
  case keyCode of
    38 -> Just Up
    40 -> Just Down
    37 -> Just Left
    39 -> Just Right
    119 -> Just Up
    115 -> Just Down
    97 -> Just Left
    100 -> Just Right
    _ -> Nothing

keyDirections : Signal Direction
keyDirections =
  Signal.filterMap getKeyDirection Up Keyboard.downs

main : Signal Html
main =
  Signal.map view <| Signal.foldp update initState keyDirections
