module Types
    exposing
        ( Story
        , Comment
        , User
        , Kids(..)
        , Collapsible(..)
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
    , text : String
    , score : Int
    , user : String
    , time : Int
    , kids : Kids
    }


type Kids
    = Kids (List Comment)


type Collapsible
    = Open
    | Closed


type alias User =
    { id : String
    , created : Int
    , about : Maybe String
    }
