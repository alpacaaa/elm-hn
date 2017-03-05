module Stories exposing (view)

import Html exposing (..)


-- import Html.Events exposing (..)

import Html.Attributes exposing (class)
import Html.Events
import Json.Decode
import Time exposing (Time)
import Erl as Url
import Types exposing (..)
import Utils exposing (maybeRender, formatTime)


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
    li [ class "ListItem" ] <| itemContent ctx story


itemContent : StoriesContext -> Story -> List (Html Msg)
itemContent { now } story =
    [ div [ class "Item__title" ]
        [ storyTitle story
        , text " "
        , maybeRender renderHost story.url
        ]
    , div [ class "Item__meta" ]
        [ span [ class "Item__score" ] [ text <| (toString story.score) ++ " points" ]
        , text " "
        , span [ class "Item__by" ]
            [ a (href (linkToUser story.user)) [ text story.user ]
            ]
        , text " "
        , time [ class "Item__time" ] [ text <| formatTime now story.time ]
        , maybeRender (\_ -> text " | ") story.commentsCount
        , maybeRender (renderCommentsCount story.id) story.commentsCount
        ]
    ]


renderHost : String -> Html.Html Msg
renderHost url =
    let
        hostParts =
            Url.extractHost url
                |> String.split "."

        host =
            String.join "." <| List.drop (List.length hostParts - 2) hostParts
    in
        span [ class "Item__host" ] [ text <| "(" ++ host ++ ")" ]


linkToStory : String -> String
linkToStory id =
    "/story/" ++ id


linkToUser : String -> String
linkToUser id =
    "/user/" ++ id


href : String -> List (Html.Attribute Msg)
href path =
    [ Html.Attributes.href path
    , Html.Events.onWithOptions
        "click"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map (\_ -> Go path) Json.Decode.value)
    ]


storyTitle : Story -> Html Msg
storyTitle story =
    let
        url =
            Maybe.withDefault (linkToStory story.id) story.url

        link =
            Maybe.map (\external -> [ Html.Attributes.href external ]) story.url
                |> Maybe.withDefault (href url)
    in
        a link [ text story.title ]


renderCommentsCount : String -> Int -> Html Msg
renderCommentsCount id comments =
    let
        str =
            if comments == 0 then
                "discuss"
            else
                toString comments ++ " comments"
    in
        a (href (linkToStory id)) [ text str ]


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


view : StoriesContext -> List Story -> Html Msg
view ctx stories =
    div [ class "Items" ]
        [ ol [ class "Items__list", listStartAttribute ctx.page ] <|
            List.map
                (listItemNews ctx)
                stories
        , paginator ctx.page
        ]
