module UserProfile exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Types exposing (Context, User)


aboutSection : String -> List (Html a)
aboutSection _ =
    [ dt [] [ text "About" ]
    , dd [] [ text "something" ]
    ]


page : Context -> User -> Html a
page { now } user =
    let
        about =
            Maybe.map aboutSection (Just "")
                |> Maybe.withDefault [ text "" ]
    in
        div [ class "UserProfile" ]
            [ h4 [] [ text "somedude" ]
            , dl [] <|
                [ dt [] [ text "Created" ]
                , dd [] [ text "some date" ]
                , dt [] [ text "Karma" ]
                , dd [] [ text "200" ]
                ]
                    ++ about
            ]
