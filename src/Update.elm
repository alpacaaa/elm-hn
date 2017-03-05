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
