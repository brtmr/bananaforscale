module Model exposing (Flags, Model, Msg(..), startModel)

import Browser.Dom exposing (Viewport)
import Notes


type alias Model =
    { viewport : Maybe Viewport
    , scale : Notes.Scale
    , root : Notes.Note
    , drawHeadstock : Bool
    , drawScalefactor : Float
    , frets : Int
    }


startModel : Model
startModel =
    { viewport = Nothing
    , scale = Notes.majorScale
    , root = Notes.E
    , drawHeadstock = True
    , drawScalefactor = 2.5
    , frets = 16
    }


type Msg
    = ViewportChange Viewport
    | WindowResize
    | NumFretsInc
    | NumFretsDec
    | ScaleSelected String


type alias Flags =
    ()
