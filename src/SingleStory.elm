module SingleStory exposing (page, renderStory)

import Erl as Url
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Set
import Time exposing (Time)
import Types exposing (..)
import Utils exposing (formatTime, innerHtml, maybeRender)


type alias SingleStoryContext =
    { now : Time
    , collapsedComments : Set.Set String
    }


page : SingleStoryContext -> Story -> Html Msg
page ctx story =
    div [ class "Items" ]
        [ ol [ class "Items__list" ] [ itemDetail ctx story ]
        ]


renderStory : { a | now : Time } -> Story -> List (Html Msg)
renderStory { now } story =
    [ div [ class "Item__title" ]
        [ storyTitle story
        , text " "
        , maybeRender renderHost story.url
        ]
    , div [ class "Item__meta" ]
        [ span [ class "Item__score" ] [ text <| toString story.score ++ " points" ]
        , text " "
        , span [ class "Item__by" ]
            [ a (Utils.href (linkToUser story.user)) [ text story.user ]
            ]
        , text " "
        , time [ class "Item__time" ] [ text <| formatTime now story.time ]
        , maybeRender (\_ -> text " | ") story.commentsCount
        , maybeRender (renderCommentsCount story.id) story.commentsCount
        ]
    , maybeRender (\text -> div [ class "Item__text" ] [ div [ innerHtml text ] [] ]) story.text
    , maybeRender renderPoll story.poll
    ]


itemDetail : SingleStoryContext -> Story -> Html Msg
itemDetail ctx story =
    div [ class "Item" ]
        [ div [ class "Item__content" ] <| renderStory ctx story
        , div [ class "Item__kids" ] <| commentsTree ctx story
        ]


renderHost : String -> Html Msg
renderHost url =
    let
        hostParts =
            Url.extractHost url
                |> String.split "."

        host =
            String.join "." <| List.drop (List.length hostParts - 2) hostParts
    in
    if String.length host > 0 then
        span [ class "Item__host" ] [ text <| "(" ++ host ++ ")" ]
    else
        text ""


linkToStory : String -> String
linkToStory id =
    "/story/" ++ id


linkToUser : String -> String
linkToUser id =
    "/user/" ++ id


storyTitle : Story -> Html Msg
storyTitle story =
    let
        url =
            Maybe.withDefault (linkToStory story.id) story.url

        link =
            Maybe.unwrap (Utils.href url) (\external -> [ href external ]) story.url
    in
    a link [ text story.title ]


collapsible : String -> Collapsible -> Html Msg
collapsible id collapsed =
    let
        symbol =
            case collapsed of
                Open ->
                    "–"

                Closed ->
                    "+"

        wrapped =
            "[" ++ symbol ++ "]"
    in
    span
        [ class "Comment__collapse", onClick (ToggleCollapse id collapsed) ]
        [ text wrapped ]


renderCommentsCount : String -> Int -> Html Msg
renderCommentsCount id comments =
    let
        str =
            if comments == 0 then
                "discuss"
            else
                toString comments ++ " comments"
    in
    a (Utils.href (linkToStory id)) [ text str ]


commentsTree : SingleStoryContext -> Story -> List (Html Msg)
commentsTree ctx story =
    List.map (singleComment ctx 0) story.comments


kids : Kids -> List Comment
kids (Kids comments) =
    comments


singleComment : SingleStoryContext -> Int -> Comment -> Html Msg
singleComment ctx level comment =
    let
        comments =
            kids comment.kids

        isCollapsed =
            Set.member comment.id ctx.collapsedComments

        collapsed =
            if isCollapsed then
                Closed
            else
                Open

        collapsedClass =
            case collapsed of
                Open ->
                    ""

                Closed ->
                    "Comment--collapsed"

        newLevel =
            singleComment ctx (level + 1)

        levelClass =
            "Comment--level" ++ toString level

        classes =
            [ "Comment", levelClass, collapsedClass ]
                |> String.join " "
    in
    div [ class classes ]
        [ div [ class "Comment__content" ]
            [ commentMeta ctx comment collapsed
            , commentText comment
            ]
        , div [ class "Comment__kids" ] <| List.map newLevel comments
        ]


commentMeta : SingleStoryContext -> Comment -> Collapsible -> Html Msg
commentMeta { now } comment collapsed =
    let
        link =
            "https://news.ycombinator.com/item?id=" ++ comment.id
    in
    div [ class "Comment__meta" ]
        [ collapsible comment.id collapsed
        , text " "
        , a (Utils.href (linkToUser comment.user) ++ [ class "Comment__user" ]) [ text comment.user ]
        , text " "
        , a [ href link ]
            [ time [] [ text <| formatTime now comment.time ]
            ]
        ]


commentText : Comment -> Html Msg
commentText comment =
    let
        link =
            "https://news.ycombinator.com/reply?id=" ++ comment.id
    in
    div [ class "Comment__text" ]
        [ div [ innerHtml comment.text ] []
        , p []
            [ a [ href link ] [ text "reply" ]
            ]
        ]


renderPoll : List PollOption -> Html Msg
renderPoll poll =
    div [ class "Item__poll" ] <| List.map renderPollOption poll


renderPollOption : PollOption -> Html Msg
renderPollOption option =
    div [ class "PollOption" ]
        [ div [ class "PollOption__text", innerHtml option.text ] []
        , div [ class "PollOption__score" ] [ text <| toString option.score ++ " points" ]
        ]
