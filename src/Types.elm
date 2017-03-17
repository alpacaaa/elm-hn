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
        , StoryType(..)
        , StoryList
        , PollOption
        )

import Set
import Time exposing (Time)
import Http
import RemoteData exposing (WebData)


type alias Model =
    { now : Time
    , route : Route
    , error : Maybe String
    }


type alias Context =
    { now : Time
    , collapsedComments : Set.Set String
    , page : Int
    }


type alias StoryList =
    { page : Int, stories : WebData (List Story) }


type Route
    = StoriesPageRoute StoryType StoryList
    | StoryRoute { id : String, story : WebData Story, collapsedComments : Set.Set String }
    | UserRoute { id : String, user : WebData User }
    | NotFoundRoute


type Msg
    = NoOp
    | FetchHNStories StoryType (WebData (List Story))
    | FetchHNStory (WebData Story)
    | FetchHNUser (WebData User)
    | CurrentTime Time
    | RouteUpdate Route
    | Go String
    | ToggleCollapse String Collapsible


type alias Story =
    { id : String
    , title : String
    , text : Maybe String
    , score : Int
    , user : String
    , time : Int
    , commentsCount : Maybe Int
    , comments : List Comment
    , url : Maybe String
    , deleted : Bool
    , dead : Bool
    , poll : Maybe (List PollOption)
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


type StoryType
    = Top
    | Newest
    | Show
    | Ask
    | Jobs


type alias PollOption =
    { text : String
    , score : Int
    }
