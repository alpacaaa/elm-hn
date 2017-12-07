module UserProfile exposing (page)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Maybe.Extra as Maybe
import Time exposing (Time)
import Types exposing (..)
import Utils exposing (formatTime, innerHtml)


page : Time -> User -> Html Msg
page now user =
    let
        about =
            Maybe.unwrap [ text "" ] aboutSection user.about
                |> p []
    in
    div [ class "Comment__text UserProfile" ]
        [ h4 [] [ text user.id ]
        , dl [] <|
            [ dt [] [ text "Created" ]
            , dd [] [ text <| formatTime now user.created ]
            , about
            , span [] (links user.id)
            ]
        ]


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
    div [] [ a [ href url ] [ text title ] ]
