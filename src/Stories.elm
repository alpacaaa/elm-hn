module Stories exposing (page)

import Html exposing (..)
import Html.Attributes exposing (class)
import Time exposing (Time)
import Types exposing (..)
import SingleStory


type alias StoriesContext =
    { now : Time
    , page : Int
    }


listStartAttribute : Int -> Html.Attribute a
listStartAttribute page =
    let
        index =
            (page - 1) * 30 + 1
    in
        Html.Attributes.start index


listItemNews : StoriesContext -> Story -> Html Msg
listItemNews ctx story =
    li [ class "ListItem" ] <| SingleStory.renderStory ctx story


paginator : Int -> Html Msg
paginator page =
    let
        next =
            "?page=" ++ (toString <| page + 1)
    in
        div [ class "Paginator" ]
            [ span [ class "Paginator__next" ]
                [ a [ Html.Attributes.href next ] [ text "More" ]
                ]
            ]


page : StoriesContext -> List Story -> Html Msg
page ctx stories =
    div [ class "Items" ]
        [ ol [ class "Items__list", listStartAttribute ctx.page ] <|
            List.map
                (listItemNews ctx)
                stories
        , paginator ctx.page
        ]
