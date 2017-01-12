module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style, src, width, height, alt)
import Http
import Types exposing (Story)
import Api
import Erl as Url


type alias Model =
    { stories : List Story
    }


init : ( Model, Cmd Msg )
init =
    let
        fetchTopStories =
            Http.send FetchHNTopStories Api.fetchTopStories
    in
        { stories = [] } ! [ fetchTopStories ]


type Msg
    = NoOp
    | FetchHNTopStories (Result Http.Error (List Story))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchHNTopStories (Ok stories) ->
            { stories = stories } ! []

        FetchHNTopStories (Err err) ->
            let
                _ =
                    Debug.log "request blew up" err
            in
                model ! []

        -- TODO
        _ ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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


emptyDiv : Html Msg
emptyDiv =
    div [] []


maybeRender : (a -> Html Msg) -> Maybe a -> Html Msg
maybeRender fn maybeValue =
    Maybe.map fn maybeValue
        |> Maybe.withDefault emptyDiv


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


listItemNews : Story -> Html Msg
listItemNews story =
    li [ class "ListItem" ] <| itemContent story


itemContent : Story -> List (Html Msg)
itemContent story =
    [ div [ class "Item__title" ]
        [ a [] [ text story.title ]
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
        , time [ class "Item__time" ] [ text "2 hours ago" ]
        , text " | "
        , a [] [ text " 133 comments" ]
        ]
    ]


itemDetail : Story -> Html Msg
itemDetail story =
    div [ class "Item" ]
        [ div [ class "Item__content" ] <| itemContent story
        , div [ class "Item__kids" ] commentsTree
        ]


collapsible : Html Msg
collapsible =
    span [ class "Comment__collapse" ] [ text "[+]" ]


commentMetaDead : Html Msg
commentMetaDead =
    div [ class "Comment__meta" ]
        [ collapsible
        , text " "
        , text " | (2 children"
        , text ", "
        , em [] [ text "5 new" ]
        , text ")"
        ]


commentMeta : Html Msg
commentMeta =
    div [ class "Comment__meta" ]
        [ collapsible
        , text " "
        , a [ class "Comment__user" ] [ text "some dudette" ]
        , time [] [ text "5 hours ago" ]
        , text " | "
        , a [] [ text "link" ]
        ]


commentText : Html Msg
commentText =
    div [ class "Comment__text" ]
        [ div []
            [ text "This is depressing. Trello is a beloved software for a lot of people. It's sad that Trello decided to sell off to Atlassian. I can't believe the same company that makes Jira is going to run Trello. SourceTree is the only software that they make that doesn't suck."
            ]
        , p []
            [ a [] [ text "reply" ]
            ]
        ]


singleComment : Html Msg
singleComment =
    div [ class "Comment Comment--level0" ]
        [ div [ class "Comment__content" ]
            [ commentMetaDead
            , commentMeta
            , commentText
            ]
        , div [ class "Comment__kids" ] []
        ]


commentsTree : List (Html Msg)
commentsTree =
    [ singleComment ]


mainContent : List Story -> Html Msg
mainContent stories =
    div [ class "Items" ]
        [ ol [ class "Items__list" ] <|
            List.map
                listItemNews
                stories
        , paginator
          -- , itemDetail
        ]


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ div [ class "App__wrap" ]
            [ div [ class "App__header" ]
                [ a [ class "App__homelinkicon" ]
                    [ img [ src "https://react-hn.appspot.com/img/logo.png", width 16, height 16, alt "" ] []
                    ]
                , a [ class "App__homelink" ]
                    [ text "React HN" ]
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
                [ mainContent model.stories
                ]
            , div [ class "App__footer" ]
                [ a [] [ text "elm-hn" ]
                ]
            ]
        ]
