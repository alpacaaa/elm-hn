module Types
    exposing
        ( Story
        )


type alias Story =
    { id : String
    , title : String
    , url : Maybe String
    }
