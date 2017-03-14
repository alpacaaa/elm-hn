module App exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style, src, width, height, alt)
import Erl as Url
import Set
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode
import Types exposing (..)
import UserProfile
import Stories
import Utils exposing (formatTime, innerHtml, maybeRender)


href : String -> List (Html.Attribute Msg)
href path =
    [ Html.Attributes.href path
    , Html.Events.onWithOptions
        "click"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map (\_ -> Go path) Json.Decode.value)
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
        span [ class "Item__host" ] [ text <| "(" ++ host ++ ")" ]


linkToStory : String -> String
linkToStory id =
    "/story/" ++ id


linkToUser : String -> String
linkToUser id =
    "/user/" ++ id


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


spinnerBouncer : Html Msg
spinnerBouncer =
    div [ class "bounce1", style [ ( "width", "6px" ), ( "height", "6px" ) ] ] []


spinner : Html Msg
spinner =
    div [ class "Spinner" ]
        [ spinnerBouncer
        , spinnerBouncer
        , spinnerBouncer
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


itemContent : Context -> Story -> List (Html Msg)
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


itemDetail : Context -> Story -> Html Msg
itemDetail ctx story =
    div [ class "Item" ]
        [ div [ class "Item__content" ] <| itemContent ctx story
        , div [ class "Item__kids" ] <| commentsTree ctx story
        ]


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
        span [ class "Comment__collapse", onClick <| ToggleCollapse id collapsed ]
            [ text wrapped ]


commentMeta : Context -> Comment -> Collapsible -> Html Msg
commentMeta { now } comment collapsed =
    let
        link =
            "https://news.ycombinator.com/item?id=" ++ comment.id
    in
        div [ class "Comment__meta" ]
            [ collapsible comment.id collapsed
            , text " "
            , a [ class "Comment__user" ] [ text comment.user ]
            , text " "
            , a [ Html.Attributes.href link ]
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
                [ a [ Html.Attributes.href link ] [ text "reply" ]
                ]
            ]


kids : Kids -> List Comment
kids (Kids comments) =
    comments


singleComment : Context -> Int -> Comment -> Html Msg
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


commentsTree : Context -> Story -> List (Html Msg)
commentsTree ctx story =
    List.map (singleComment ctx 0) story.comments


storyMainContent : Context -> Story -> Html Msg
storyMainContent ctx story =
    div [ class "Items" ]
        [ ol [ class "Items__list" ]
            [ itemDetail ctx story
            ]
        ]


notFound : Html Msg
notFound =
    div [] [ text "Not found" ]


loadingView : Html Msg
loadingView =
    div [ class "Items" ]
        [ ol [ class "Items__list" ]
            [ div [ class "Item" ]
                [ div [ class "Item__title" ]
                    [ text "Loading..." ]
                ]
            ]
        ]


remoteContent : WebData a -> (a -> Html Msg) -> Html Msg
remoteContent data createHtml =
    case data of
        NotAsked ->
            text ""

        Loading ->
            loadingView

        Success a ->
            createHtml a

        Failure err ->
            text "some error :("


mainContent : Model -> Html Msg
mainContent model =
    let
        ctx =
            { now = model.now
            , collapsedComments = Set.empty
            , page = 1
            }
    in
        case model.route of
            HomeRoute { page, stories } ->
                remoteContent stories (Stories.view { now = model.now, page = page })

            StoryRoute { story, collapsedComments } ->
                remoteContent story (storyMainContent { ctx | collapsedComments = collapsedComments })

            UserRoute { user } ->
                remoteContent user (UserProfile.page ctx)

            _ ->
                notFound


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ div [ class "App__wrap" ]
            [ div [ class "App__header" ]
                [ a [ class "App__homelinkicon" ]
                    [ img [ src "https://react-hn.appspot.com/img/logo.png", width 16, height 16, alt "" ] []
                    ]
                , a (href "/" ++ [ class "App__homelink" ])
                    [ text "Elm HN" ]
                , a (href "/newest") [ text "new" ]
                , text " | "
                , a (href "/show") [ text "show" ]
                , text " | "
                , a (href "/ask") [ text "ask" ]
                , text " | "
                , a (href "/jobs") [ text "jobs" ]
                , a [ class "App__settings" ]
                    [ text "settings"
                    ]
                ]
            , div [ class "App__content" ]
                [ mainContent model
                ]
            , div [ class "App__footer" ]
                [ a [] [ text "elm-hn" ]
                ]
            ]
        ]
