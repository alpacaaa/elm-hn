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
            { now = 0, route = currentRoute }

        cmds =
            cmdsForRoute currentRoute
    in
        initialModel ! (currentTime :: cmds)


cmdsForRoute : Route -> List (Cmd Msg)
cmdsForRoute route =
    case route of
        TopStoriesRoute { page } ->
            [ Http.send FetchHNStories <| Api.fetchStories Top ((page - 1) * 30) ]

        StoryRoute { id } ->
            [ Http.send FetchHNStory <| Api.fetchStory id ]

        UserRoute { id } ->
            [ Http.send FetchHNUser <| Api.fetchUser id ]

        _ ->
            []


routeByLocation : Navigation.Location -> Route
routeByLocation loc =
    let
        parsed =
            Url.parse loc.href
    in
        case parsed.path of
            [] ->
                TopStoriesRoute
                    { page = (getPage parsed.query)
                    , stories = Loading
                    }

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        CurrentTime time ->
            { model | now = time } ! []

        RouteUpdate route ->
            { model | route = route } ! cmdsForRoute route

        FetchHNStories (Ok stories) ->
            case model.route of
                TopStoriesRoute data ->
                    let
                        newRoute =
                            TopStoriesRoute { data | stories = Success stories }
                    in
                        { model | route = newRoute } ! []

                _ ->
                    Debug.crash "impossible"

        FetchHNStories (Err err) ->
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
            Debug.log "request blew up" err
    in
        model ! []
