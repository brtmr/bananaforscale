module Model exposing (Flags, Model, Msg(..), startModel)

import Browser.Dom exposing (Viewport)
import Notes


type alias Model =
    { viewport : Maybe Viewport
    , scale : Notes.Scale
    , drawHeadstock : Bool
    }


startModel : Model
startModel =
    { viewport = Nothing
    , scale = Notes.majorScale
    , drawHeadstock = True
    }


type Msg
    = ViewportChange Viewport
    | WindowResize


type alias Flags =
    ()
