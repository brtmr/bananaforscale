module Banana exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Debug
import Dict
import Drawing exposing (fretBoard)
import DrawingMath
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (decodeString, int)
import List
import List.Extra exposing (zip, zip3)
import Model exposing (..)
import Notes
import Task



-- view model = svg [width "800", height "100"] []


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( startModel, Task.perform ViewportChange getViewport )



-- Update


toggle : Maybe Bool -> Maybe Bool
toggle =
    Maybe.andThen (\x -> Just <| not x)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewportChange newViewport ->
            ( recalculate { model | viewport = Just newViewport }, Cmd.none )

        WindowResize ->
            ( model, Task.perform ViewportChange getViewport )

        NumFretsInc ->
            if model.frets < 24 then
                ( recalculate { model | frets = model.frets + 1 }, Cmd.none )

            else
                ( recalculate model, Cmd.none )

        NumFretsDec ->
            if model.frets > 5 then
                ( recalculate { model | frets = model.frets - 1 }, Cmd.none )

            else
                ( recalculate model, Cmd.none )

        ScaleSelected s ->
            case s of
                "major" ->
                    ( Model.switchScale model Notes.majorScale, Cmd.none )

                "majorPentatonic" ->
                    ( Model.switchScale model Notes.majorPentatonicScale, Cmd.none )

                "minor" ->
                    ( Model.switchScale model Notes.minorScale, Cmd.none )

                "minorPentatonic" ->
                    ( Model.switchScale model Notes.minorPentatonicScale, Cmd.none )

                "blues" ->
                    ( Model.switchScale model Notes.bluesScale, Cmd.none )

                "bbKing" ->
                    ( Model.switchScale model Notes.bbKingScale, Cmd.none )

                "harmonicMajor" ->
                    ( Model.switchScale model Notes.harmonicMajorScale, Cmd.none )

                "harmonicMinor" ->
                    ( Model.switchScale model Notes.harmonicMinorScale, Cmd.none )

                "dorian" ->
                    ( Model.switchScale model Notes.dorianScale, Cmd.none )

                "phrygian" ->
                    ( Model.switchScale model Notes.phrygianScale, Cmd.none )

                "lydian" ->
                    ( Model.switchScale model Notes.lydianScale, Cmd.none )

                "mixolydian" ->
                    ( Model.switchScale model Notes.mixolydianScale, Cmd.none )

                "locrian" ->
                    ( Model.switchScale model Notes.locrianScale, Cmd.none )

                _ ->
                    ( Model.switchScale model Notes.majorScale, Cmd.none )

        RootSelected r ->
            case decodeString int r of
                Result.Ok n ->
                    ( Model.switchRoot model <| Notes.intToNote n, Cmd.none )

                Result.Err _ ->
                    ( model, Cmd.none )

        NoteSelection s ->
            case decodeString int s of
                Result.Ok n ->
                    ( { model | selectedNotes = Dict.update n toggle model.selectedNotes }, Cmd.none )

                Result.Err _ ->
                    ( model, Cmd.none )



-- View


getNoteCssClass : Model -> Notes.Note -> String
getNoteCssClass m n =
    let
        num =
            Notes.scaleDegreeAsInt m.root n + 1

        numString =
            String.fromInt num
    in
    case Dict.get (Notes.noteToInt n) m.selectedNotes of
        Just True ->
            "note" ++ numString

        _ ->
            "nonote"


scaleDisplay : Model -> List (Html Msg)
scaleDisplay m =
    let
        notes =
            List.map Notes.intToNote <|
                List.map (modBy 12) <|
                    List.range (0 + Notes.noteToInt m.root)
                        (11 + Notes.noteToInt m.root)

        notesWithDegrees =
            List.map (\n -> Notes.contextualizeNote m.root n) notes

        scale =
            Notes.makeScaleWithDegrees m.root m.scale

        names =
            List.map Notes.noteName notes

        classes =
            List.map (\n -> getNoteCssClass m n) notes

        shorten t =
            if Tuple.first t == Tuple.second t then
                Tuple.first t

            else
                Tuple.first t ++ "/" ++ Tuple.second t

        shortnames =
            List.map shorten names

        notesClassesNames =
            zip3 notesWithDegrees classes shortnames

        toDiv ( note, class_, name ) =
            div
                [ class class_
                , onClick (NoteSelection (String.fromInt <| Notes.noteToInt <| note.note))
                ]
                [ Html.input
                    [ type_ "checkbox"
                    , value (String.fromInt <| Notes.noteToInt <| note.note)
                    , checked
                        (case
                            Dict.get (Notes.noteToInt note.note) m.selectedNotes
                         of
                            Just b ->
                                b

                            Nothing ->
                                False
                        )
                    ]
                    []
                , Html.text <| name ++ "(" ++ note.degree ++ ")"
                ]
    in
    List.map toDiv notesClassesNames


noteOptions : List (Html Msg)
noteOptions =
    let
        notes =
            List.map Notes.intToNote (List.range 0 11)

        names =
            List.map Notes.noteName notes

        shorten t =
            if Tuple.first t == Tuple.second t then
                Tuple.first t

            else
                Tuple.first t ++ "/" ++ Tuple.second t

        shortnames =
            List.map shorten names

        numberedShortnames =
            zip (List.range 0 11) shortnames

        toOption ( index, name ) =
            option [ value <| String.fromInt index ] [ text name ]
    in
    List.map toOption numberedShortnames


body : Model -> List (Html Msg)
body m =
    let
        vpWidth =
            case m.viewport of
                Nothing ->
                    700.0

                Just vp ->
                    vp.viewport.width

        svgWidth =
            vpWidth * 0.99

        svgHeight =
            150.0

        mobile =
            vpWidth < 1000

        klass cls =
            if mobile then
                class <| cls ++ " mobile"

            else
                class cls
    in
    [ div
        [ id "settings" ]
        [ div
            [ klass "setting" ]
            [ span
                []
                [ Html.text <| String.fromInt m.frets ++ " Frets" ]
            , button [ onClick Model.NumFretsDec ]
                [ Html.text "-" ]
            , button
                [ onClick Model.NumFretsInc ]
                [ Html.text "+" ]
            ]
        , div
            [ klass "setting" ]
            [ span
                []
                [ text "Root:  " ]
            , select [ id "rootselect", onInput RootSelected ] noteOptions
            ]
        , div
            [ klass "setting" ]
            [ span
                []
                [ text "Scale:  " ]
            , select [ id "scaleselect", onInput ScaleSelected ]
                [ option [ value "major" ] [ text "Major (Ionian)" ]
                , option [ value "majorPentatonic" ] [ text "Major Pentatonic" ]
                , option [ value "minor" ] [ text "Minor (Aeolian)" ]
                , option [ value "minorPentatonic" ] [ text "Minor Pentatonic" ]
                , option [ value "blues" ] [ text "Blues" ]
                , option [ value "bbKing" ] [ text "B.B. King" ]
                , option [ value "harmonicMajor" ] [ text "Harmonic Major" ]
                , option [ value "harmonicMinor" ] [ text "Harmonic Minor" ]
                , option [ value "dorian" ] [ text "Dorian" ]
                , option [ value "phrygian" ] [ text "Phrygian" ]
                , option [ value "lydian" ] [ text "Lydian" ]
                , option [ value "mixolydian" ] [ text "Mixolydian" ]
                , option [ value "locrian" ] [ text "Locrian" ]
                ]
            ]
        ]
    , div
        [ klass "setting", id "scale" ]
        (scaleDisplay m)
    , div [ id "content" ]
        [ fretBoard m
        ]
    , div [ id "footer" ]
        [ div []
            [ Html.text "Source "
            , a [ href "https://github.com/brtmr/bananaforscale" ] [ Html.text "here" ]
            ]
        , a
            [ href "https://github.com/brtmr/bananaforscale", class "title" ]
            [ Html.img [ src "banana.png", height 50 ] []
            , h1 [] [ Html.text "for scale" ]
            ]
        , div []
            [ Html.text "Made with "
            , a [ href "https://elm-lang.org" ] [ Html.text "elm" ]
            ]
        ]
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Banana for Scale", body = body model }



-- Subscriptions


subscriptions _ =
    onResize (\_ _ -> WindowResize)



-- Application


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
