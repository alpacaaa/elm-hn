module Types
    exposing
        ( Story
        )


type alias Story =
    { id : String
    , title : String
    , score : Int
    , user : String
    , url : Maybe String
    }
