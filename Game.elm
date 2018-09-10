module Game exposing (game)

import Html exposing (Html, div, text, button, ul, li, beginnerProgram)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Array exposing (Array)
import Maybe exposing (Maybe)

import Square exposing (..)
import Board exposing (renderBoard)
import Utils exposing (hasWinner, squaresValueAt)

game = beginnerProgram { model = initialModel, update = update, view = view }

-- MODEL
type alias StepModel = 
  { squares : Array Square
  , nextMark : Square.Mark
  }

type alias Model = List StepModel

initialModel : Model
initialModel =
  [ { squares = Array.initialize 9 (always Square.Empty)
    , nextMark = Square.X
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
    Just {squares, nextMark} ->
      div 
        [ styles.body ]
        [ div
          [ styles.game ]
          [ renderBoard squares ClickSquare ]
        , div
            [ styles.gameInfo ] 
            [ div 
                [ styles.status ]
                [ text <| getStatus squares nextMark ]
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
        (won, _)  = hasWinner winningLines stepModel.squares
        proceed = not won && squaresValueAt index stepModel.squares == Square.Empty
      in
        if proceed then
          { squares = Array.set index (Square.Marked stepModel.nextMark) stepModel.squares
          , nextMark = if stepModel.nextMark == Square.X then Square.O else Square.X
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


getStatus : Array Square -> Square.Mark -> String
getStatus squares nextMark =
  case hasWinner winningLines squares of
    (True, winner)  -> 
      "Winner: " ++ Square.displayMark winner
    (False, _) -> 
      if isGameOver squares then 
        "Game Over !!!" 
      else 
        "Next player: " ++ toString nextMark


isGameOver : Array Square -> Bool
isGameOver squares = 
  Array.isEmpty <| Array.filter ((==) Square.Empty) squares


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