module Api
    exposing
        ( fetchTopStories
        , fetchStory
        )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Pipeline
import Types exposing (Story, Comment)


type alias Field =
    { name : String
    , args : Args
    , query : Query
    }


type Query
    = Query (List Field)


type alias Args =
    List ( String, String )


field : String -> List Field -> Field
field name fields =
    Field name [] (Query fields)


fieldWithArgs : String -> Args -> List Field -> Field
fieldWithArgs name args fields =
    Field name args (Query fields)


queryToString : Query -> String
queryToString (Query query) =
    if List.isEmpty query then
        ""
    else
        let
            str =
                List.map fieldToString query
                    |> List.foldr (++) ""
        in
            "{ " ++ str ++ " }"


fieldToString : Field -> String
fieldToString { name, args, query } =
    name ++ " " ++ argsToString args ++ " " ++ queryToString query


argsToString : Args -> String
argsToString args =
    if List.isEmpty args then
        ""
    else
        let
            transform =
                \( key, value ) -> key ++ ": " ++ value

            str =
                List.map transform args
                    |> List.foldr (++) " "
        in
            "( " ++ str ++ " )"


storyDecoder : Decode.Decoder Story
storyDecoder =
    Pipeline.decode Story
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "score" Decode.int
        |> Pipeline.requiredAt [ "by", "id" ] Decode.string
        |> Pipeline.required "time" Decode.int
        |> Pipeline.optional "descendants" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "kids" (Decode.list commentDecoder) []
        |> Pipeline.optional "url" (Decode.nullable Decode.string) Nothing


commentDecoder : Decode.Decoder Comment
commentDecoder =
    Pipeline.decode Comment
        |> Pipeline.required "id" Decode.string


topStoriesQuery : Query
topStoriesQuery =
    Query
        [ field "hn"
            [ field "topStories"
                [ field "id" []
                , field "url" []
                , field "title" []
                , field "score" []
                , field "time" []
                , field "by"
                    [ field "id" []
                    ]
                , field "descendants" []
                ]
            ]
        ]


fetchTopStories : Http.Request (List Story)
fetchTopStories =
    let
        query =
            queryToString topStoriesQuery

        body =
            Encode.object
                [ ( "query", Encode.string query ) ]

        jsonBody =
            Http.jsonBody <| body

        decoder =
            Decode.at [ "data", "hn", "topStories" ] <|
                Decode.list storyDecoder
    in
        Http.post "https://www.graphqlhub.com/graphql" jsonBody decoder


commentFields : List Field
commentFields =
    [ field "id" []
    , field "text" []
    , field "score" []
    , field "time" []
    , field "by"
        [ field "id" []
        ]
    ]


fetchKids : Int -> Field
fetchKids count =
    let
        fields =
            if count == 0 then
                commentFields
            else
                fetchKids (count - 1) :: commentFields
    in
        field "kids" fields


storyQuery : String -> Query
storyQuery id =
    Query
        [ field "hn"
            [ fieldWithArgs "item"
                [ ( "id", id ) ]
                [ field "id" []
                , field "url" []
                , field "title" []
                , field "score" []
                , field "time" []
                , field "by"
                    [ field "id" []
                    ]
                , fetchKids 5
                ]
            ]
        ]


fetchStory : String -> Http.Request Story
fetchStory id =
    let
        query =
            queryToString <| storyQuery id

        body =
            Encode.object
                [ ( "query", Encode.string query ) ]

        jsonBody =
            Http.jsonBody <| body

        decoder =
            Decode.at [ "data", "hn", "item" ] storyDecoder
    in
        Http.post "https://www.graphqlhub.com/graphql" jsonBody decoder
