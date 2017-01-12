module Types
    exposing
        ( Story
        )


type alias Story =
    { id : String
    , title : String
    , score : Int
    , user : String
    , time : Int
    , url : Maybe String
    }
