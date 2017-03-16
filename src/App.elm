module App exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style, src, width, height, alt)
import Erl as Url
import Set
import RemoteData exposing (RemoteData(..), WebData)
import Json.Decode
import Time
import Types exposing (..)
import UserProfile
import Stories
import Utils exposing (formatTime, innerHtml, maybeRender)


type alias HeaderLinkConfig =
    { storyType : StoryType
    , path : String
    , text : String
    , class : String
    }


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
    , maybeRender (\text -> div [ class "Item__text" ] [ div [ innerHtml text ] [] ]) story.text
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
            , a (href (linkToUser comment.user) ++ [ class "Comment__user" ]) [ text comment.user ]
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


notFoundView : Html Msg
notFoundView =
    genericView
        [ div [ class "Item__title" ]
            [ text "Not Found" ]
        ]


genericView : List (Html Msg) -> Html Msg
genericView content =
    div [ class "Items" ]
        [ ol [ class "Items__list" ]
            [ div [ class "Item" ] content
            ]
        ]


loadingView : Html Msg
loadingView =
    genericView
        [ div [ class "Item__title" ] [ text "Loading..." ]
        , p [] [ text "api is a bit slow, hold on." ]
        ]


errorView : String -> Html Msg
errorView err =
    genericView
        [ text "Good News! Something blew up ¯\\_(ツ)_/¯ Here's the error."
        , pre []
            [ code [] [ text err ]
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


remoteConentStories : StoryList -> Time.Time -> Html Msg
remoteConentStories { page, stories } now =
    remoteContent stories (Stories.view { now = now, page = page })


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
            TopStoriesRoute data ->
                remoteConentStories data model.now

            NewestStoriesRoute data ->
                remoteConentStories data model.now

            ShowStoriesRoute data ->
                remoteConentStories data model.now

            AskStoriesRoute data ->
                remoteConentStories data model.now

            JobsStoriesRoute data ->
                remoteConentStories data model.now

            StoryRoute { story, collapsedComments } ->
                remoteContent story (storyMainContent { ctx | collapsedComments = collapsedComments })

            UserRoute { user } ->
                remoteContent user (UserProfile.page ctx)

            NotFoundRoute ->
                notFoundView


renderAppOrError : Model -> Html Msg
renderAppOrError model =
    case model.error of
        Nothing ->
            mainContent model

        Just err ->
            errorView err


headerLink : Maybe StoryType -> HeaderLinkConfig -> Html Msg
headerLink activeStoryType config =
    let
        link =
            href config.path

        isActive =
            Maybe.map ((==) config.storyType) activeStoryType
                |> Maybe.withDefault False

        classes =
            config.class
                ++ (if isActive then
                        " active"
                    else
                        ""
                   )
    in
        a (link ++ [ class classes ]) [ text config.text ]


routeToStoryType : Route -> Maybe StoryType
routeToStoryType route =
    case route of
        TopStoriesRoute _ ->
            Just Top

        NewestStoriesRoute _ ->
            Just Newest

        ShowStoriesRoute _ ->
            Just Show

        AskStoriesRoute _ ->
            Just Ask

        JobsStoriesRoute _ ->
            Just Jobs

        StoryRoute _ ->
            Nothing

        UserRoute _ ->
            Nothing

        NotFoundRoute ->
            Nothing


header : Route -> Html Msg
header route =
    let
        activeStoryType =
            routeToStoryType route

        elmHn =
            { storyType = Top
            , path = "/"
            , class = "App__homelink"
            , text = "Elm HN"
            }

        new =
            { storyType = Newest
            , path = "/newest"
            , class = ""
            , text = "new"
            }

        show =
            { storyType = Show
            , path = "/show"
            , class = ""
            , text = "show"
            }

        ask =
            { storyType = Ask
            , path = "/ask"
            , class = ""
            , text = "ask"
            }

        jobs =
            { storyType = Jobs
            , path = "/jobs"
            , class = ""
            , text = "jobs"
            }

        navLinks =
            [ new, show, ask, jobs ]
                |> List.map (headerLink activeStoryType)
                |> List.intersperse (text " | ")
    in
        div [ class "App__header" ] <|
            [ a [ class "App__homelinkicon" ] []
            ]
                ++ [ headerLink activeStoryType elmHn ]
                ++ navLinks


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ div [ class "App__wrap" ]
            [ header model.route
            , div [ class "App__content" ]
                [ renderAppOrError model ]
            , div [ class "App__footer" ]
                [ a [ Html.Attributes.href "https://github.com/alpacaaa/elm-hn" ]
                    [ text "elm-hn" ]
                ]
            ]
        ]
