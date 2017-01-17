module Main exposing (..)

import App exposing (..)
import Types
import Navigation


main : Program Never Types.Model Types.Msg
main =
    Navigation.program onLocationChange { view = view, init = init, update = update, subscriptions = subscriptions }
