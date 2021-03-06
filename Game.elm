module Game exposing (game)

import Html exposing (Html, div, text, button, ul, li, beginnerProgram)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Array exposing (Array)
import Maybe exposing (Maybe)

import Board exposing (renderBoard)
import Utils exposing (Squares, hasWinner, squaresValueAt)

game = beginnerProgram { model = initialModel, update = update, view = view }

-- MODEL
type alias StepModel = 
  { squares : Squares
  , xIsNext : Bool
  }

type alias Model = List StepModel

initialModel : Model
initialModel =
  [ { squares = Array.initialize 9 (always Nothing)
    , xIsNext = True
    }
  ]

-- MESSAGES
type Msg = ClickSquare Int | GoToStep Int

-- UPDATE
update : Msg -> Model -> Model
update msg model = 
  case msg of 
    ClickSquare index ->
      onClickSquare index model
    GoToStep step ->
      if step == 0 then 
        initialModel 
      else 
        List.drop (List.length model - step - 1) model

-- VIEW
view : Model -> Html Msg
view model =
  case List.head model of
    Just {squares, xIsNext} ->
      div 
        [ styles.body ]
        [ div
          [ styles.game ]
          [ renderBoard squares ClickSquare ]
        , div
            [ styles.gameInfo ] 
            [ div 
                [ styles.status ]
                [ text <| getStatus squares xIsNext ]
            , ul
                [ styles.ul ]
                (renderHistory (List.length model) GoToStep)
            ]    
        ]
    Nothing ->
      div [] []


-- Helpers

onClickSquare : Int -> Model -> Model
onClickSquare index model =
  case List.head model of
    Just stepModel ->
      let
        proceed = not (hasWinner winningLines stepModel.squares) && squaresValueAt index stepModel.squares == Nothing
      in
        if proceed then
          { squares = Array.set index (Just <| nextPlayer stepModel.xIsNext) stepModel.squares
          , xIsNext = if stepModel.xIsNext then False else True
          }
          :: model
        else
          model
    Nothing ->
      model


renderHistory : Int -> (Int -> msg) -> List (Html msg)
renderHistory steps onClickItem =
  List.map (historyItem onClickItem) (List.range 0 (steps-1))


historyItem : (Int -> msg) -> Int -> Html msg
historyItem onClickItem step =
  li 
    [ onClick (onClickItem step) ] 
    [ text <| "Go to " ++ (if step == 0 then "start" else "step " ++ toString step) ]


nextPlayer : Bool -> String
nextPlayer xIsNext = 
  if xIsNext then "X" else "O"


getStatus : Squares -> Bool -> String
getStatus squares xIsNext =
  case hasWinner winningLines squares of
    True  -> 
      "Winner: " ++ nextPlayer (not xIsNext)
    False -> 
      if isGameOver squares then 
        "Game Over !!!" 
      else 
        "Next player: " ++ nextPlayer xIsNext


isGameOver : Squares -> Bool
isGameOver squares = 
  Array.isEmpty <| Array.filter ((==) Nothing) squares


winningLines : List (List Int)
winningLines =
  [ [ 0, 1, 2 ]
  , [ 3, 4, 5 ]
  , [ 6, 7, 8 ]
  , [ 0, 3, 6 ]
  , [ 1, 4, 7 ]
  , [ 2, 5, 8 ]
  , [ 0, 4, 8 ]
  , [ 2, 4, 6 ]
  ]


styles = 
  { game =
      style
        [ ("display", "flex")
        , ("flex-direction", "row")
        ]
  , gameInfo =
      style 
        [ ("margin-left", "20px") ]
  , status =
      style  
        [ ("margin-bottom", "10px") ]
  , body =
      style
        [ ("font", "14px Century Gothic, Futura, sans-serif")
        , ("margin", "20px")
        ]
  , ul = 
      style
        [ ("padding-left", "30px") ]
  }