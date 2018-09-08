module Board exposing (renderBoard)

import Html exposing (Html, div, Attribute)
import Html.Attributes exposing (style)
import Array exposing (Array)
import Maybe exposing (Maybe)

import Square exposing (..)

renderBoard : Array Square -> (Int -> msg) -> Html msg
renderBoard squares onClickSquare = 
 div
    []
    [
      div
        [ boardRowStyle ]
        [ renderSquare (squareAt 0 squares) (onClickSquare 0)
        , renderSquare (squareAt 1 squares) (onClickSquare 1)
        , renderSquare (squareAt 2 squares) (onClickSquare 2)
        ]
    , div
        [ boardRowStyle ]
        [ renderSquare (squareAt 3 squares) (onClickSquare 3)
        , renderSquare (squareAt 4 squares) (onClickSquare 4)
        , renderSquare (squareAt 5 squares) (onClickSquare 5)
        ]
    , div
        [ boardRowStyle ]
        [ renderSquare (squareAt 6 squares) (onClickSquare 6)
        , renderSquare (squareAt 7 squares) (onClickSquare 7)
        , renderSquare (squareAt 8 squares) (onClickSquare 8)
        ]
    ]

boardRowStyle : Attribute msg
boardRowStyle =
  style
    [ ("clear", "both")
    , ("content", "")
    , ("display", "table")
    ]

squareAt : Int -> Array Square -> Square
squareAt index squares =
  Maybe.withDefault Square.Empty (Array.get index squares)
