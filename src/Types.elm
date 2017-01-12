module Types
    exposing
        ( Story
        )


type alias Story =
    { id : String
    , title : String
    , score : Int
    , url : Maybe String
    }
