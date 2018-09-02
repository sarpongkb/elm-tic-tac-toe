module Main exposing (..)

import Html exposing (beginnerProgram)

import Game exposing (initialModel, update, view)

main =
  beginnerProgram { model = initialModel, update = update, view = view }