module UserProfile exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (Context, User, Msg)
import Utils exposing (formatTime, innerHtml)


aboutSection : String -> List (Html Msg)
aboutSection about =
    [ dt [] [ text "About" ]
    , dd [ innerHtml about ] []
    ]


page : Context -> User -> Html Msg
page { now } user =
    let
        about =
            Maybe.map aboutSection user.about
                |> Maybe.withDefault [ text "" ]
    in
        div [ class "UserProfile" ]
            [ h4 [] [ text user.id ]
            , dl [] <|
                [ dt [] [ text "Created" ]
                , dd [] [ text <| formatTime now user.created ]
                ]
                    ++ about
            ]
