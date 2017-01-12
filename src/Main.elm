module Main exposing (..)

import App exposing (..)
import Html exposing (program)
import Navigation


main : Program Never Model Msg
main =
    Navigation.program onLocationChange { view = view, init = init, update = update, subscriptions = subscriptions }
