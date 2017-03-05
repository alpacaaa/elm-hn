module Main exposing (..)

import App
import Types
import Update
import Navigation


main : Program Never Types.Model Types.Msg
main =
    Navigation.program Update.onLocationChange
        { view = App.view
        , init = Update.init
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
