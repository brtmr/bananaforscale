module Model exposing (Flags, Model, Msg(..), recalculate, selectEntireScale, startModel, switchRoot, switchScale)

import Browser.Dom exposing (Viewport)
import Dict
import DrawingMath
import HeadStock
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
    , coos : DrawingMath.SvgCoordinates
    }


recalculate : Model -> Model
recalculate m =
    let
        drawHeadstock =
            case m.viewport of
                Nothing ->
                    False

                Just v ->
                    (v.viewport.width - (HeadStock.nutXUnscaled * m.drawScalefactor)) / toFloat m.frets > 60

        newcoos =
            DrawingMath.calculate m.viewport
                m.frets
                m.drawScalefactor
                drawHeadstock
    in
    { m | coos = newcoos, drawHeadstock = drawHeadstock }


startModel : Model
startModel =
    { viewport = Nothing
    , scale = Notes.majorScale
    , root = Notes.C
    , tuning = NeckNotes.standardTuning
    , drawHeadstock = False
    , drawScalefactor = 5
    , frets = 16
    , selectedNotes =
        selectEntireScale Notes.C Notes.majorScale
    , coos = DrawingMath.calculate Nothing 16 5 False
    }


nothingSelected : SelectedNotes
nothingSelected =
    Dict.fromList <|
        [ ( 0, False )
        , ( 1, False )
        , ( 2, False )
        , ( 3, False )
        , ( 4, False )
        , ( 5, False )
        , ( 6, False )
        , ( 7, False )
        , ( 8, False )
        , ( 9, False )
        , ( 10, False )
        , ( 11, False )
        ]


selectEntireScale : Notes.Note -> Notes.Scale -> SelectedNotes
selectEntireScale root scale =
    let
        s =
            Dict.fromList <|
                List.map (\n -> ( Notes.noteToInt n.note, True )) <|
                    Notes.makeScaleWithDegrees root scale
    in
    Dict.union s nothingSelected


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
