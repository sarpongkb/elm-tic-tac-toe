module Square exposing (..)

import Html exposing (Html, button, text, Attribute)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

type Mark = X | O
type Square = Marked Mark | Empty

renderSquare : Square -> msg -> Html msg
renderSquare square onClickItem =
  button
    [ styles.square
    , onClick onClickItem
    ]
    [ text <| displayMark square ]  

displayMark: Square -> String
displayMark square =
  case square of
    Empty -> ""
    Marked mark -> toString mark

styles =
  { square = 
      style
        [ ("background", "#fff")
        , ("border", "1px solid #999")
        , ("float", "left")
        , ("font-size", "44px")
        , ("font-weight", "bold")
        , ("line-height", "54px")
        , ("height", "54px")
        , ("margin-right", "-1px")
        , ("margin-top", "-1px")
        , ("padding", "0")
        , ("text-align", "center")
        , ("width", "54px")
        ]
  }
