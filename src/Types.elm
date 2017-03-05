module Types
    exposing
        ( Model
        , Msg(..)
        , Route(..)
        , Context
        , Story
        , Comment
        , User
        , Kids(..)
        , Collapsible(..)
        )

import Set
import Time
import Http
import RemoteData exposing (WebData)


type alias Model =
    { stories : WebData (List Story)
    , story : WebData Story
    , user : WebData User
    , now : Time.Time
    , route : Route
    , collapsedComments : Set.Set String
    }


type alias Context =
    { now : Time.Time
    , collapsedComments : Set.Set String
    , page : Int
    }


type Route
    = HomeRoute Int
    | StoryRoute String
    | UserRoute String
    | NotFoundRoute


type Msg
    = NoOp
    | FetchHNTopStories (Result Http.Error (List Story))
    | FetchHNStory (Result Http.Error Story)
    | FetchHNUser (Result Http.Error User)
    | CurrentTime Time.Time
    | RouteUpdate Route
    | Go String
    | ToggleCollapse String Collapsible


type alias Story =
    { id : String
    , title : String
    , score : Int
    , user : String
    , time : Int
    , commentsCount : Maybe Int
    , comments : List Comment
    , url : Maybe String
    , deleted : Bool
    , dead : Bool
    }


type alias Comment =
    { id : String
    , text : String
    , score : Int
    , user : String
    , time : Int
    , kids : Kids
    , deleted : Bool
    , dead : Bool
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
