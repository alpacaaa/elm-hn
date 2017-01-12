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
    , comments : Maybe Int
    , url : Maybe String
    }
