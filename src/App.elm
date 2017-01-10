module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style, src, width, height, alt)


type alias Model =
    { message : String
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "Your Elm App is working!" }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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


listItemNews : Html Msg
listItemNews =
    li [ class "ListItem" ]
        [ div [ class "Item__title" ]
            [ a [] [ text "Trello acquires Atlassian lol" ]
            , text " "
            , span [ class "Item__host" ] [ text "(theonion.com)" ]
            ]
        , div [ class "Item__meta" ]
            [ span [ class "Item__score" ] [ text "1582 points" ]
            , text " "
            , span [ class "Item__by" ]
                [ a [] [ text "some dude" ]
                ]
            , text " "
            , time [ class "Item__time" ] [ text "2 hours ago" ]
            , text " | "
            , a [] [ text " 133 comments" ]
            ]
        ]


mainContent : Html Msg
mainContent =
    div [ class "Items" ]
        [ ol [ class "Items__list" ]
            [ listItemLoading
            , listItemNews
            , listItemNews
            , listItemNews
            ]
        , paginator
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
                [ mainContent
                ]
            , div [ class "App__footer" ]
                [ a [] [ text "elm-hn" ]
                ]
            ]
        ]
