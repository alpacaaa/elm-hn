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
            { now = 0, route = currentRoute }

        cmds =
            cmdsForRoute currentRoute
    in
        initialModel ! (currentTime :: cmds)


createRequest : (WebData a -> Msg) -> Http.Request a -> List (Cmd Msg)
createRequest msg apiCall =
    apiCall
        |> RemoteData.sendRequest
        |> Cmd.map msg
        |> List.singleton


fetchStories : StoryType -> Int -> List (Cmd Msg)
fetchStories storyType page =
    createRequest (FetchHNStories storyType) <|
        Api.fetchStories storyType ((page - 1) * 30)


cmdsForRoute : Route -> List (Cmd Msg)
cmdsForRoute route =
    case route of
        StoriesPageRoute storyType { page } ->
            fetchStories storyType page

        StoryRoute { id } ->
            createRequest FetchHNStory <|
                Api.fetchStory id

        UserRoute { id } ->
            createRequest FetchHNUser <|
                Api.fetchUser id

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
                StoriesPageRoute Top <| storiesDict ()

            "newest" :: [] ->
                StoriesPageRoute Newest <| storiesDict ()

            "show" :: [] ->
                StoriesPageRoute Show <| storiesDict ()

            "ask" :: [] ->
                StoriesPageRoute Ask <| storiesDict ()

            "jobs" :: [] ->
                StoriesPageRoute Jobs <| storiesDict ()

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

        FetchHNStories storyType response ->
            case model.route of
                StoriesPageRoute _ data ->
                    let
                        newRoute =
                            StoriesPageRoute storyType { data | stories = response }
                    in
                        { model | route = newRoute } ! []

                _ ->
                    model ! []

        FetchHNStory response ->
            case model.route of
                StoryRoute data ->
                    let
                        newRoute =
                            StoryRoute { data | story = response }
                    in
                        { model | route = newRoute } ! []

                _ ->
                    model ! []

        FetchHNUser response ->
            case model.route of
                UserRoute data ->
                    let
                        newRoute =
                            UserRoute { data | user = response }
                    in
                        { model | route = newRoute } ! []

                _ ->
                    model ! []

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
                    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
