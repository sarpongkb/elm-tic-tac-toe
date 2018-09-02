module Game exposing (initialModel, update, view)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Maybe exposing (Maybe)

import Board exposing (renderBoard)

-- MODEL
type alias Squares = Array (Maybe String)

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

    ClickSquare i ->
      case List.head model of
        Just stepModel ->
          let
            proceed = not (hasWinner winningLines stepModel.squares) && squaresValueAt i stepModel.squares == Nothing
          in
            if proceed then
              { stepModel 
              | squares = Array.set i (Just <| nextPlayer stepModel.xIsNext) stepModel.squares
              , xIsNext = if stepModel.xIsNext then False else True
              }
              :: model
            else
              model
        Nothing ->
          model

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
        []
        ( List.append 
            [ renderBoard squares ClickSquare
            , div [] [text <| statusText (hasWinner winningLines squares) xIsNext]
            ]
            (renderHistory (List.length model) GoToStep) 
        )
    Nothing ->
      div [] []

-- Helpers

renderHistory : Int -> (Int -> msg) -> List (Html msg)
renderHistory steps onClickItem =
  List.map 
    ( \i -> 
        let 
          stepText = if i == 0 then "start" else "step " ++ toString i
        in 
          (button [ onClick (onClickItem i) ] [ text <| "Go to " ++ stepText ]) 
    )
    ( List.range 0 (steps-1) )


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

nextPlayer : Bool -> String
nextPlayer xIsNext = if xIsNext then "X" else "O"

statusText : Bool -> Bool -> String
statusText isWon xIsNext =
  if isWon then
    "Winner: " ++ nextPlayer (not xIsNext)
  else
    "Next player: " ++ nextPlayer xIsNext

squaresValueAt : Int -> Squares -> Maybe String 
squaresValueAt index squares =
  Maybe.withDefault Nothing (Array.get index squares)

lineWinner : List Int -> Squares -> Maybe String
lineWinner line squares =
  let 
    mappedLine = List.map (\v -> squaresValueAt v squares) line
  in
    if List.all (\v -> v == Just "X") mappedLine then
      Just "X"
    else
      if List.all (\v -> v == Just "O") mappedLine then
        Just "O"
      else
        Nothing

hasWinner : List (List Int) -> Squares -> Bool
hasWinner lines squares =
  let
    lineWinners = List.map (\line -> lineWinner line squares) lines    
  in
    List.any (\a -> a /= Nothing) lineWinners