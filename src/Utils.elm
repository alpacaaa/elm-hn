module Utils exposing (..)

import Date
import Time
import Date.Distance
import Html
import Html.Attributes
import Json.Encode
import Html.Events
import Json.Decode
import Types exposing (..)
import Maybe.Extra as MaybeX


formatTime : Time.Time -> Int -> String
formatTime nowMs ms =
    let
        now =
            Date.fromTime nowMs

        date =
            Date.fromTime <| toFloat ms * 1000
    in
        (Date.Distance.inWords date now) ++ " ago"


innerHtml : String -> Html.Attribute Msg
innerHtml content =
    Html.Attributes.property "innerHTML" <| Json.Encode.string content


maybeRender : (a -> Html.Html b) -> Maybe a -> Html.Html b
maybeRender fn maybeValue =
    MaybeX.unwrap (Html.text "") fn maybeValue


href : String -> List (Html.Attribute Msg)
href path =
    [ Html.Attributes.href path
    , Html.Events.onWithOptions
        "click"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map (\_ -> Go path) Json.Decode.value)
    ]
