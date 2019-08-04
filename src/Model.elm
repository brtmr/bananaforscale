module Model exposing (Flags, Model, Msg(..), selectEntireScale, startModel, switchRoot, switchScale)

import Browser.Dom exposing (Viewport)
import Dict
import NeckNotes
import Notes


type alias SelectedNotes =
    Dict.Dict Int Bool


type alias Model =
    { viewport : Maybe Viewport
    , scale : Notes.Scale
    , root : Notes.Note
    , tuning : NeckNotes.Tuning
    , drawHeadstock : Bool
    , drawScalefactor : Float
    , frets : Int
    , selectedNotes : SelectedNotes
    }


startModel : Model
startModel =
    { viewport = Nothing
    , scale = Notes.majorScale
    , root = Notes.C
    , tuning = NeckNotes.standardTuning
    , drawHeadstock = True
    , drawScalefactor = 5
    , frets = 16
    , selectedNotes =
        selectEntireScale Notes.C Notes.majorScale
    }


selectEntireScale : Notes.Note -> Notes.Scale -> SelectedNotes
selectEntireScale root scale =
    Dict.fromList <|
        List.map (\n -> ( Notes.noteToInt n, True )) <|
            Notes.makeScale root scale


switchScale : Model -> Notes.Scale -> Model
switchScale model scale =
    { model
        | scale = scale
        , selectedNotes = selectEntireScale model.root scale
    }


switchRoot : Model -> Notes.Note -> Model
switchRoot model root =
    { model
        | root = root
        , selectedNotes = selectEntireScale root model.scale
    }


type Msg
    = ViewportChange Viewport
    | WindowResize
    | NumFretsInc
    | NumFretsDec
    | ScaleSelected String
    | RootSelected String
    | NoteSelection String


type alias Flags =
    ()
