module Utils exposing (hasWinner, squaresValueAt)

import Maybe exposing (Maybe)
import Array exposing (Array)

import Square exposing (..)

hasWinner : List (List Int) -> Array Square -> (Bool, Square)
hasWinner winningLines squares =
  List.map (\line -> lineWinner line squares) winningLines
    |> List.filter (\(hw, sq) -> hw == True)
    |> List.head
    |> Maybe.withDefault (False, Square.Empty)


lineWinner : List Int -> Array Square -> (Bool, Square)
lineWinner line squares =
  let 
    mappedLine = List.map (\v -> squaresValueAt v squares) line
  in
    if List.all ((==) (Square.Marked X)) mappedLine then
      (True, Square.Marked X)
    else
      if List.all ((==) (Square.Marked O)) mappedLine then
        (True, Square.Marked O)
      else
        (False, Square.Empty)

squaresValueAt : Int -> Array Square -> Square 
squaresValueAt index squares = 
  Maybe.withDefault Square.Empty (Array.get index squares)
