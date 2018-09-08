module Utils exposing (hasWinner, squaresValueAt)

import Maybe exposing (Maybe)
import Array exposing (Array)

import Square exposing (..)

hasWinner : List (List Int) -> Array Square -> Bool
hasWinner winningLines squares =
  List.map (\line -> lineWinner line squares) winningLines
    |>  List.any (\winner -> winner /= Square.Empty)


lineWinner : List Int -> Array Square -> Square
lineWinner line squares =
  let 
    mappedLine = List.map (\v -> squaresValueAt v squares) line
  in
    if List.all ((==) Square.X) mappedLine then
      X
    else
      if List.all ((==) Square.O) mappedLine then
        O
      else
        Square.Empty

squaresValueAt : Int -> Array Square -> Square 
squaresValueAt index squares = 
  Maybe.withDefault Square.Empty (Array.get index squares)
