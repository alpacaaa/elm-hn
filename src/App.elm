module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style, src, width, height, alt)
import Date
import Time
import Task
import Http
import Set
import Types exposing (Story, Comment, Kids(..), Collapsible(..))
import Api
import Erl as Url
import Date.Distance
import Navigation
import Json.Decode
import Json.Encode


type alias Model =
    { stories : List Story
    , story : Maybe Story
    , now : Time.Time
    , route : Route
    , collapsedComments : Set.Set String
    }


type alias Context =
    { now : Time.Time
    , collapsedComments : Set.Set String
    }


type Route
    = Home
    | Story String
    | NotFound


type Msg
    = NoOp
    | FetchHNTopStories (Result Http.Error (List Story))
    | FetchHNStory (Result Http.Error Story)
    | CurrentTime Time.Time
    | RouteUpdate Route
    | Go String
    | ToggleCollapse String Collapsible


onLocationChange : Navigation.Location -> Msg
onLocationChange loc =
    RouteUpdate <| routeByLocation loc


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        currentTime =
            Task.perform CurrentTime Time.now

        initialModel =
            { stories = []
            , story = Nothing
            , now = 0
            , route = Home
            , collapsedComments = Set.empty
            }

        currentRoute =
            routeByLocation location

        cmds =
            if currentRoute /= initialModel.route then
                [ Navigation.newUrl location.pathname ]
            else
                cmdsForRoute currentRoute
    in
        initialModel ! (currentTime :: cmds)


cmdsForRoute : Route -> List (Cmd Msg)
cmdsForRoute route =
    case route of
        Home ->
            [ Http.send FetchHNTopStories Api.fetchTopStories ]

        Story id ->
            [ Http.send FetchHNStory <| Api.fetchStory id ]

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
                Home

            "story" :: id :: [] ->
                Story id

            _ ->
                NotFound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        CurrentTime time ->
            { model | now = time } ! []

        RouteUpdate route ->
            { model | route = route } ! cmdsForRoute route

        FetchHNTopStories (Ok stories) ->
            { model | stories = stories } ! []

        FetchHNTopStories (Err err) ->
            logErr model err

        FetchHNStory (Ok story) ->
            { model | story = Just story } ! []

        FetchHNStory (Err err) ->
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
    "story/" ++ id


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


formatTime : Time.Time -> Int -> String
formatTime nowMs ms =
    let
        now =
            Date.fromTime nowMs

        date =
            Date.fromTime <| toFloat ms * 1000
    in
        (Date.Distance.inWords date now) ++ " ago"


innerHtml : String -> Html.Attribute Msg
innerHtml content =
    Html.Attributes.property "innerHTML" <| Json.Encode.string content


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
            [ a [] [ text story.user ]
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


mainContent : Model -> Html Msg
mainContent model =
    let
        ctx =
            { now = model.now
            , collapsedComments = model.collapsedComments
            }
    in
        case model.route of
            Home ->
                homeMainContent ctx model.stories

            Story _ ->
                Maybe.map (storyMainContent ctx) model.story
                    |> Maybe.withDefault notFound

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
