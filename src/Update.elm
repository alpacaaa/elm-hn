module Update exposing (init, update, subscriptions, onLocationChange)

import Time
import Task
import Http
import Set
import Erl as Url
import Navigation
import RemoteData exposing (WebData, RemoteData(..))
import Api
import Types exposing (..)


onLocationChange : Navigation.Location -> Msg
onLocationChange loc =
    RouteUpdate <| routeByLocation loc


toggleCollapseHelper :
    comparable
    -> Collapsible
    -> Set.Set comparable
    -> Set.Set comparable
toggleCollapseHelper id state comments =
    let
        operation =
            case state of
                Open ->
                    Set.insert

                Closed ->
                    Set.remove
    in
        operation id comments


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        currentTime =
            Task.perform CurrentTime Time.now

        currentRoute =
            routeByLocation location

        defaultModel =
            { now = 0 }

        initialModel =
            { now = 0, route = currentRoute, error = Nothing }

        cmds =
            cmdsForRoute currentRoute
    in
        initialModel ! (currentTime :: cmds)


fetchStories : StoryType -> Int -> List (Cmd Msg)
fetchStories storyType page =
    [ Http.send (FetchHNStories storyType) <| Api.fetchStories storyType ((page - 1) * 30) ]


cmdsForRoute : Route -> List (Cmd Msg)
cmdsForRoute route =
    case route of
        TopStoriesRoute { page } ->
            fetchStories Top page

        NewestStoriesRoute { page } ->
            fetchStories Newest page

        ShowStoriesRoute { page } ->
            fetchStories Show page

        AskStoriesRoute { page } ->
            fetchStories Ask page

        JobsStoriesRoute { page } ->
            fetchStories Jobs page

        StoryRoute { id } ->
            [ Http.send FetchHNStory <| Api.fetchStory id ]

        UserRoute { id } ->
            [ Http.send FetchHNUser <| Api.fetchUser id ]

        NotFoundRoute ->
            []


routeByLocation : Navigation.Location -> Route
routeByLocation loc =
    let
        parsed =
            Url.parse loc.href

        storiesDict () =
            { page = (getPage parsed.query)
            , stories = Loading
            }
    in
        case parsed.path of
            [] ->
                TopStoriesRoute <| storiesDict ()

            "newest" :: [] ->
                NewestStoriesRoute <| storiesDict ()

            "show" :: [] ->
                ShowStoriesRoute <| storiesDict ()

            "ask" :: [] ->
                AskStoriesRoute <| storiesDict ()

            "jobs" :: [] ->
                JobsStoriesRoute <| storiesDict ()

            "story" :: id :: [] ->
                StoryRoute
                    { id = id
                    , story = Loading
                    , collapsedComments = Set.empty
                    }

            "user" :: id :: [] ->
                UserRoute
                    { id = id
                    , user = Loading
                    }

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


updateRouteModelWithStories :
    Model
    -> StoryList
    -> List Story
    -> (StoryList -> Route)
    -> ( Model, Cmd a )
updateRouteModelWithStories model data stories route =
    let
        newRoute =
            route { data | stories = Success stories }
    in
        { model | route = newRoute } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        CurrentTime time ->
            { model | now = time } ! []

        RouteUpdate route ->
            { model | route = route } ! cmdsForRoute route

        FetchHNStories storyType (Ok stories) ->
            case storyType of
                Top ->
                    case model.route of
                        TopStoriesRoute data ->
                            updateRouteModelWithStories model data stories TopStoriesRoute

                        _ ->
                            Debug.crash "impossible"

                Newest ->
                    case model.route of
                        NewestStoriesRoute data ->
                            updateRouteModelWithStories model data stories NewestStoriesRoute

                        _ ->
                            Debug.crash "impossible"

                Show ->
                    case model.route of
                        NewestStoriesRoute data ->
                            updateRouteModelWithStories model data stories ShowStoriesRoute

                        _ ->
                            Debug.crash "impossible"

                Ask ->
                    case model.route of
                        AskStoriesRoute data ->
                            updateRouteModelWithStories model data stories AskStoriesRoute

                        _ ->
                            Debug.crash "impossible"

                Jobs ->
                    case model.route of
                        JobsStoriesRoute data ->
                            updateRouteModelWithStories model data stories JobsStoriesRoute

                        _ ->
                            Debug.crash "impossible"

        FetchHNStories route (Err err) ->
            logErr model err

        FetchHNStory (Ok story) ->
            case model.route of
                StoryRoute data ->
                    let
                        newRoute =
                            StoryRoute { data | story = (Success story) }
                    in
                        { model | route = newRoute } ! []

                _ ->
                    Debug.crash "impossible"

        FetchHNStory (Err err) ->
            logErr model err

        FetchHNUser (Ok user) ->
            case model.route of
                UserRoute data ->
                    let
                        newRoute =
                            UserRoute { data | user = (Success user) }
                    in
                        { model | route = newRoute } ! []

                _ ->
                    Debug.crash "impossible"

        FetchHNUser (Err err) ->
            logErr model err

        Go path ->
            ( model, Navigation.newUrl path )

        ToggleCollapse id collapsed ->
            case model.route of
                StoryRoute data ->
                    let
                        newCollapsed =
                            toggleCollapseHelper id collapsed data.collapsedComments

                        newRoute =
                            StoryRoute { data | collapsedComments = newCollapsed }
                    in
                        { model | route = newRoute } ! []

                _ ->
                    Debug.crash "impossible"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


logErr : Model -> Http.Error -> ( Model, Cmd a )
logErr model err =
    let
        _ =
            Debug.log "Doh" err
    in
        { model | error = Just (toString err) } ! []
