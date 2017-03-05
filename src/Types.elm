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
import Time exposing (Time)
import Http
import RemoteData exposing (WebData)


type alias Model =
    { now : Time
    , route : Route
    }


type alias Context =
    { now : Time
    , collapsedComments : Set.Set String
    , page : Int
    }


type Route
    = HomeRoute { page : Int, stories : WebData (List Story) }
    | StoryRoute { id : String, story : WebData Story, collapsedComments : Set.Set String }
    | UserRoute { id : String, user : WebData User }
    | NotFoundRoute


type Msg
    = NoOp
    | FetchHNTopStories (Result Http.Error (List Story))
    | FetchHNStory (Result Http.Error Story)
    | FetchHNUser (Result Http.Error User)
    | CurrentTime Time
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
