module App exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, style, src, width, height, alt)
import RemoteData exposing (RemoteData(..), WebData)
import Time
import Types exposing (..)
import UserProfile
import SingleStory
import Stories
import Utils exposing (formatTime, innerHtml, maybeRender, href)
import Maybe.Extra as MaybeX


type alias HeaderLinkConfig =
    { storyType : StoryType
    , path : String
    , text : String
    , class : String
    }


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
            errorView (toString err)


remoteConentStories : StoryList -> Time.Time -> Html Msg
remoteConentStories { page, stories } now =
    remoteContent stories (Stories.view { now = now, page = page })


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


headerLink : Maybe StoryType -> HeaderLinkConfig -> Html Msg
headerLink activeStoryType config =
    let
        link =
            href config.path

        isActive =
            MaybeX.unwrap False ((==) config.storyType) activeStoryType

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
        StoriesPageRoute storyType _ ->
            Just storyType

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
                [ mainContent model ]
            , div [ class "App__footer" ]
                [ a [ Html.Attributes.href "https://github.com/alpacaaa/elm-hn" ]
                    [ text "elm-hn" ]
                ]
            ]
        ]
