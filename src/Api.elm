module Api
    exposing
        ( fetchTopStories
        )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Pipeline
import Types exposing (Story)


type alias Field =
    { name : String
    , query : Query
    }


type Query
    = Query (List Field)


field : String -> List Field -> Field
field name fields =
    Field name (Query fields)


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
fieldToString { name, query } =
    name ++ " " ++ queryToString query


storyDecoder : Decode.Decoder Story
storyDecoder =
    Pipeline.decode Story
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "url" (Decode.nullable Decode.string)


topStoriesQuery : Query
topStoriesQuery =
    Query
        [ field "hn"
            [ field "topStories"
                [ field "id" []
                , field "url" []
                , field "title" []
                , field "score" []
                , field "kids"
                    [ field "id" []
                    ]
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
