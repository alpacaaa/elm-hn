module Api
    exposing
        ( fetchStories
        , fetchStory
        , fetchUser
        )

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Types exposing (..)


type alias Field =
    { name : String
    , args : Args
    , query : Query
    }


type Query
    = Query (List Field)


type alias Args =
    List ( String, String )


storiesQuery : StoryType -> Int -> Query
storiesQuery storyType offset =
    Query
        [ field "hn"
            [ fieldWithArgs (storyTypeString storyType)
                [ ( "offset", toString offset ), ( "limit", "30" ) ]
                [ field "id" []
                , field "url" []
                , field "title" []
                , field "score" []
                , field "time" []
                , field "by"
                    [ field "id" []
                    ]
                , field "descendants" []
                , field "deleted" []
                , field "dead" []
                ]
            ]
        ]


storyQuery : String -> Query
storyQuery id =
    Query
        [ field "hn"
            [ fieldWithArgs "item"
                [ ( "id", id ) ]
                [ field "id" []
                , field "url" []
                , field "title" []
                , field "text" []
                , field "score" []
                , field "time" []
                , field "by"
                    [ field "id" []
                    ]
                , fetchKids 5
                , field "deleted" []
                , field "dead" []
                , field "parts"
                    [ field "text" []
                    , field "score" []
                    ]
                ]
            ]
        ]


userQuery : String -> Query
userQuery id =
    Query
        [ field "hn"
            [ fieldWithArgs "user"
                [ ( "id", toString id ) ]
                [ field "id" []
                , field "created" []
                , field "about" []
                ]
            ]
        ]


commentFields : List Field
commentFields =
    [ field "id" []
    , field "text" []
    , field "score" []
    , field "time" []
    , field "by"
        [ field "id" []
        ]
    , field "deleted" []
    , field "dead" []
    ]


runQuery : Query -> Decode.Decoder a -> String -> Http.Request a
runQuery query decoder returnField =
    let
        queryStr =
            queryToString query

        jsonBody =
            Http.jsonBody <|
                Encode.object
                    [ ( "query", Encode.string queryStr ) ]

        finalDecoder =
            Decode.at [ "data", "hn", returnField ] decoder
    in
    Http.post "https://www.graphqlhub.com/graphql" jsonBody finalDecoder


fetchStories : StoryType -> Int -> Http.Request (List Story)
fetchStories storyType offset =
    let
        query =
            storiesQuery storyType offset

        skipNull =
            Decode.map Maybe.values

        decoder =
            Decode.list (Decode.nullable storyDecoder)
                |> skipNull
                |> skipDeleted
    in
    runQuery query decoder (storyTypeString storyType)


fetchStory : String -> Http.Request Story
fetchStory id =
    runQuery (storyQuery id) storyDecoder "item"


fetchUser : String -> Http.Request User
fetchUser id =
    runQuery (userQuery id) userDecoder "user"


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


skipDeleted :
    Decode.Decoder (List { a | deleted : Bool, dead : Bool })
    -> Decode.Decoder (List { a | deleted : Bool, dead : Bool })
skipDeleted =
    let
        skip =
            List.filter (\{ deleted, dead } -> not <| deleted || dead)
    in
    Decode.map skip


storyDecoder : Decode.Decoder Story
storyDecoder =
    Pipeline.decode Story
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "title" Decode.string
        |> Pipeline.optional "text" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "score" Decode.int 0
        |> Pipeline.optionalAt [ "by", "id" ] Decode.string ""
        |> Pipeline.optional "time" Decode.int 0
        |> Pipeline.optional "descendants" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "kids" (Decode.list commentDecoder |> skipDeleted) []
        |> Pipeline.optional "url" (Decode.nullable Decode.string) Nothing
        |> Pipeline.optional "deleted" Decode.bool False
        |> Pipeline.optional "dead" Decode.bool False
        |> Pipeline.optional "parts" (Decode.nullable <| Decode.list pollDecoder) Nothing


kidsDecoder : Decode.Decoder Kids
kidsDecoder =
    Decode.lazy (\_ -> Decode.list commentDecoder |> skipDeleted |> Decode.map Kids)


commentDecoder : Decode.Decoder Comment
commentDecoder =
    Pipeline.decode Comment
        |> Pipeline.required "id" Decode.string
        |> Pipeline.optional "text" Decode.string ""
        |> Pipeline.optional "score" Decode.int 0
        |> Pipeline.optionalAt [ "by", "id" ] Decode.string ""
        |> Pipeline.optional "time" Decode.int 0
        |> Pipeline.optional "kids" kidsDecoder (Kids [])
        |> Pipeline.optional "deleted" Decode.bool False
        |> Pipeline.optional "dead" Decode.bool False


userDecoder : Decode.Decoder User
userDecoder =
    Pipeline.decode User
        |> Pipeline.required "id" Decode.string
        |> Pipeline.optional "created" Decode.int 0
        |> Pipeline.optional "about" (Decode.nullable Decode.string) Nothing


pollDecoder : Decode.Decoder PollOption
pollDecoder =
    Pipeline.decode PollOption
        |> Pipeline.required "text" Decode.string
        |> Pipeline.required "score" Decode.int


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


storyTypeString : StoryType -> String
storyTypeString storyType =
    case storyType of
        Top ->
            "topStories"

        Newest ->
            "newStories"

        Show ->
            "showStories"

        Ask ->
            "askStories"

        Jobs ->
            "jobStories"
