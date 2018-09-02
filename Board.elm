module Board exposing (renderBoard)

import Html exposing (Html, div, Attribute)
import Html.Attributes exposing (style)
import Array exposing (Array)
import Maybe exposing (Maybe)

import Square exposing ( renderSquare )

renderBoard : Array (Maybe String) -> (Int -> msg) -> Html msg
renderBoard squares onClickSquare = 
 div
    []
    [
      div
        [ boardRowStyle ]
        [ renderSquare (squareValueAt 0 squares) (onClickSquare 0)
        , renderSquare (squareValueAt 1 squares) (onClickSquare 1)
        , renderSquare (squareValueAt 2 squares) (onClickSquare 2)
        ]
    , div
        [ boardRowStyle ]
        [ renderSquare (squareValueAt 3 squares) (onClickSquare 3)
        , renderSquare (squareValueAt 4 squares) (onClickSquare 4)
        , renderSquare (squareValueAt 5 squares) (onClickSquare 5)
        ]
    , div
        [ boardRowStyle ]
        [ renderSquare (squareValueAt 6 squares) (onClickSquare 6)
        , renderSquare (squareValueAt 7 squares) (onClickSquare 7)
        , renderSquare (squareValueAt 8 squares) (onClickSquare 8)
        ]
    ]

boardRowStyle : Attribute msg
boardRowStyle =
  style
    [ ("clear", "both")
    , ("content", "")
    , ("display", "table")
    ]

squareValueAt : Int -> Array (Maybe String) -> Maybe String
squareValueAt index squares =
  Maybe.withDefault Nothing (Array.get index squares)
