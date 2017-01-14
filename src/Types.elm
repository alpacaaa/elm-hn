module Types
    exposing
        ( Story
        , Comment
        )


type alias Story =
    { id : String
    , title : String
    , score : Int
    , user : String
    , time : Int
    , commentsCount : Maybe Int
    , comments : List Comment
    , url : Maybe String
    }


type alias Comment =
    { id : String
    }
