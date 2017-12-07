module App exposing (view)

import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, height, href, src, style, width)
import Maybe.Extra as Maybe
import RemoteData exposing (RemoteData(..), WebData)
import SingleStory
import Stories
import Time exposing (Time)
import Types exposing (..)
import UserProfile
import Utils exposing (formatTime, innerHtml, maybeRender)


type alias HeaderLinkConfig =
    { storyType : StoryType
    , path : String
    , text : String
    , class : String
    }


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ div [ class "App__wrap" ]
            [ header model.route
            , div [ class "App__content" ]
                [ mainContent model ]
            , div [ class "App__footer" ]
                [ a [ href "https://github.com/alpacaaa/elm-hn" ] [ text "elm-hn" ] ]
            ]
        ]


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
    div [ class "App__header" ]
        [ a [ class "App__homelinkicon" ] []
        , headerLink activeStoryType elmHn
        , span [] navLinks
        ]


routeToStoryType : Route -> Maybe StoryType
routeToStoryType route =
    case route of
        StoriesPageRoute storyType _ ->
            Just storyType

        StoryRoute _ ->
            Nothing

        UserRoute _ ->
            Nothing

        NotFoundRoute ->
            Nothing


headerLink : Maybe StoryType -> HeaderLinkConfig -> Html Msg
headerLink activeStoryType config =
    let
        link =
            Utils.href config.path

        isActive =
            Maybe.unwrap False ((==) config.storyType) activeStoryType

        classes =
            classList
                [ ( config.class, True )
                , ( "active", isActive )
                ]
    in
    a (link ++ [ classes ]) [ text config.text ]


mainContent : Model -> Html Msg
mainContent model =
    case model.route of
        StoriesPageRoute storyType data ->
            remoteConentStories data model.now

        StoryRoute { story, collapsedComments } ->
            remoteContent story (SingleStory.page { now = model.now, collapsedComments = collapsedComments })

        UserRoute { user } ->
            remoteContent user (UserProfile.page model.now)

        NotFoundRoute ->
            notFoundView


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
            errorView (toString err)


remoteConentStories : StoryList -> Time -> Html Msg
remoteConentStories { page, stories } now =
    remoteContent stories (Stories.page { now = now, page = page })


genericView : List (Html Msg) -> Html Msg
genericView content =
    div [ class "Items" ]
        [ ol [ class "Items__list" ]
            [ div [ class "Item" ] content ]
        ]


notFoundView : Html Msg
notFoundView =
    genericView
        [ div [ class "Item__title" ] [ text "Not Found" ]
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
        , pre [] [ code [] [ text err ] ]
        ]
