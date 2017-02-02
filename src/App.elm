module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style, src, width, height, alt)
import Time
import Task
import Http
import Set
import Erl as Url
import Navigation
import RemoteData exposing (WebData, RemoteData(..))
import Json.Decode
import Api
import Types exposing (Model, Msg(..), Route(..), Context, Story, Comment, User, Kids(..), Collapsible(..))
import UserProfile
import Utils exposing (formatTime, innerHtml)


onLocationChange : Navigation.Location -> Msg
onLocationChange loc =
    RouteUpdate <| routeByLocation loc


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        currentTime =
            Task.perform CurrentTime Time.now

        currentRoute =
            routeByLocation location

        defaultModel =
            { stories = NotAsked
            , story = NotAsked
            , user = NotAsked
            , now = 0
            , route = currentRoute
            , collapsedComments = Set.empty
            }

        initialModel =
            loadStatus defaultModel currentRoute

        cmds =
            cmdsForRoute currentRoute
    in
        initialModel ! (currentTime :: cmds)


loadStatus : Model -> Route -> Model
loadStatus model route =
    case route of
        HomeRoute _ ->
            { model | stories = Loading }

        StoryRoute _ ->
            { model | story = Loading }

        UserRoute _ ->
            { model | user = Loading }

        _ ->
            model


cmdsForRoute : Route -> List (Cmd Msg)
cmdsForRoute route =
    case route of
        HomeRoute page ->
            [ Http.send FetchHNTopStories <| Api.fetchTopStories ((page - 1) * 30) ]

        StoryRoute id ->
            [ Http.send FetchHNStory <| Api.fetchStory id ]

        UserRoute id ->
            [ Http.send FetchHNUser <| Api.fetchUser id ]

        _ ->
            []


href : String -> List (Html.Attribute Msg)
href path =
    [ Html.Attributes.href path
    , Html.Events.onWithOptions
        "click"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map (\_ -> Go path) Json.Decode.value)
    ]


routeByLocation : Navigation.Location -> Route
routeByLocation loc =
    let
        parsed =
            Url.parse loc.href
    in
        case parsed.path of
            [] ->
                HomeRoute (getPage parsed.query)

            "story" :: id :: [] ->
                StoryRoute id

            "user" :: id :: [] ->
                UserRoute id

            _ ->
                NotFoundRoute


getPage : Url.Query -> Int
getPage query =
    query
        |> List.filterMap getPageHelper
        |> List.head
        |> Maybe.withDefault 1


getPageHelper : ( String, String ) -> Maybe Int
getPageHelper ( key, val ) =
    if (key == "page") then
        Result.toMaybe <| String.toInt val
    else
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        CurrentTime time ->
            { model | now = time } ! []

        RouteUpdate route ->
            let
                newModel =
                    loadStatus { model | route = route } route
            in
                newModel ! cmdsForRoute route

        FetchHNTopStories (Ok stories) ->
            { model | stories = Success stories } ! []

        FetchHNTopStories (Err err) ->
            logErr model err

        FetchHNStory (Ok story) ->
            { model | story = Success story } ! []

        FetchHNStory (Err err) ->
            logErr model err

        FetchHNUser (Ok user) ->
            { model | user = Success user } ! []

        FetchHNUser (Err err) ->
            logErr model err

        Go path ->
            ( model, Navigation.newUrl path )

        ToggleCollapse id collapsed ->
            toggleCollapseHelper id collapsed model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


logErr : Model -> Http.Error -> ( Model, Cmd a )
logErr model err =
    let
        _ =
            Debug.log "request blew up" err
    in
        model ! []


toggleCollapseHelper : String -> Collapsible -> Model -> Model
toggleCollapseHelper id state model =
    let
        operation =
            case state of
                Open ->
                    Set.insert

                Closed ->
                    Set.remove
    in
        { model
            | collapsedComments = operation id model.collapsedComments
        }


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


maybeRender : (a -> Html Msg) -> Maybe a -> Html Msg
maybeRender fn maybeValue =
    Maybe.map fn maybeValue
        |> Maybe.withDefault (text "")


paginator : Html Msg
paginator =
    div [ class "Paginator" ]
        [ span [ class "Paginator__prev" ]
            [ a [] [ text "Prev" ]
            ]
        , text " | "
        , span [ class "Paginator__next" ]
            [ a [] [ text "More" ]
            ]
        ]


listItemLoading : Html Msg
listItemLoading =
    li [ class "ListItem ListItem--loading" ]
        [ spinner
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


listItemNews : Context -> Story -> Html Msg
listItemNews ctx story =
    li [ class "ListItem" ] <| itemContent ctx story


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
    div [ class "Comment__meta" ]
        [ collapsible comment.id collapsed
        , text " "
        , a [ class "Comment__user" ] [ text comment.user ]
        , text " "
        , time [] [ text <| formatTime now comment.time ]
        , text " | "
        , a [] [ text "link" ]
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


homeMainContent : Context -> List Story -> Html Msg
homeMainContent ctx stories =
    div [ class "Items" ]
        [ ol [ class "Items__list" ] <|
            List.map
                (listItemNews ctx)
                stories
        , paginator
          -- , itemDetail
        ]


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


remoteContent : WebData a -> (a -> Html Msg) -> Html Msg
remoteContent data createHtml =
    case data of
        NotAsked ->
            text ""

        Loading ->
            text "Loading"

        Success a ->
            createHtml a

        Failure err ->
            text "some error :("


mainContent : Model -> Html Msg
mainContent model =
    let
        ctx =
            { now = model.now
            , collapsedComments = model.collapsedComments
            }
    in
        case model.route of
            HomeRoute page ->
                remoteContent model.stories (homeMainContent ctx)

            StoryRoute _ ->
                remoteContent model.story (storyMainContent ctx)

            UserRoute _ ->
                remoteContent model.user (UserProfile.page ctx)

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
                , a [ class "App__homelink" ]
                    [ text "Elm HN" ]
                , a [] [ text "new" ]
                , text " | "
                , a [] [ text "comments" ]
                , text " | "
                , a [] [ text "show" ]
                , text " | "
                , a [] [ text "ask" ]
                , text " | "
                , a [] [ text "jobs" ]
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
