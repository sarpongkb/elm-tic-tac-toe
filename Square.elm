module Square exposing (renderSquare)

import Html exposing (Html, button, text, Attribute)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

renderSquare : Maybe String -> msg -> Html msg
renderSquare value onClickItem =
  let 
    displayedValue = 
      case value of
        Nothing -> 
          "" 
        Just v -> 
          v
  in
    button
      [ styles.square
      , onClick onClickItem
      ]
      [ text displayedValue ]  

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
