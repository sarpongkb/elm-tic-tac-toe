module Utils exposing (Squares, hasWinner, squaresValueAt)

import Maybe exposing (Maybe)
import Array exposing (Array)

type alias Squares = Array (Maybe String)

hasWinner : List (List Int) -> Squares -> Bool
hasWinner winningLines squares =
  List.map (\line -> lineWinner line squares) winningLines
    |>  List.any (\winner -> winner /= Nothing)


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

squaresValueAt : Int -> Squares -> Maybe String 
squaresValueAt index squares = 
  Maybe.withDefault Nothing (Array.get index squares)
