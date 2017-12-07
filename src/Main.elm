module Main exposing (..)

import App
import Navigation
import Types
import Update


main : Program Never Types.Model Types.Msg
main =
    Navigation.program Update.onLocationChange
        { view = App.view
        , init = Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
