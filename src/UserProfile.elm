module UserProfile exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Types exposing (Context, User, Msg)
import Utils exposing (formatTime, innerHtml)


aboutSection : String -> List (Html Msg)
aboutSection about =
    [ dt [] [ text "About" ]
    , dd [ innerHtml about ] []
    ]


links : String -> List (Html Msg)
links id =
    [ dt [] []
    , dd [] <|
        List.map (makeLink id)
            [ ( "submitted", "stories" )
            , ( "threads", "comments" )
            , ( "favorites", "favorites" )
            ]
    ]


makeLink : String -> ( String, String ) -> Html Msg
makeLink id ( slug, title ) =
    let
        url =
            String.join "" [ "https://news.ycombinator.com/", slug, "?id=", id ]
    in
        div []
            [ a [ href url ] [ text title ]
            ]


page : Context -> User -> Html Msg
page { now } user =
    let
        about =
            Maybe.map aboutSection user.about
                |> Maybe.withDefault [ text "" ]
    in
        div [ class "Comment__text UserProfile" ]
            [ h4 [] [ text user.id ]
            , dl [] <|
                [ dt [] [ text "Created" ]
                , dd [] [ text <| formatTime now user.created ]
                ]
                    ++ about
                    ++ links user.id
            ]
