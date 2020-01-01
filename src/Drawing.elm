module Drawing exposing (fretBoard, getNoteCssClass, singleFret, singleString)

import Dict exposing (Dict)
import DrawingMath
import Html exposing (..)
import List.Extra exposing (zip)
import Model exposing (..)
import NeckNotes
import Notes
import Scale as S
import Svg exposing (..)
import Svg.Attributes exposing (..)


backgroundColor =
    "#333333ff"


fretboardColor =
    "#222"


fretColor =
    "#ccccccff"


stringColor =
    "#ffffff"


intervalRatio =
    1.059463


singleFret : Model -> Int -> Svg msg
singleFret model fret =
    let
        x =
            S.convert model.coos.fretScale <| toFloat fret

        halfFretBack =
            String.fromFloat <| -0.5 * model.coos.fretWidth fret

        textHeight =
            String.fromFloat <| model.coos.neckHeight + 35
    in
    Svg.g
        [ transform <| "translate(" ++ String.fromFloat x ++ ",0)"
        ]
        [ rect
            [ width "3"
            , height <| String.fromFloat model.coos.neckHeight
            , fill fretColor
            ]
            []
        , Svg.text_
            [ Svg.Attributes.transform <| "translate(" ++ halfFretBack ++ ", " ++ textHeight ++ ")"
            , Svg.Attributes.fill "#999"
            ]
            [ Svg.text <| String.fromInt fret ]
        ]


singleString : Model -> Int -> Svg msg
singleString model nString =
    let
        h =
            1

        yPos =
            S.convert model.coos.stringScale (toFloat nString) - (h / 2)
    in
    rect
        [ height <| String.fromFloat h
        , width <| String.fromFloat model.coos.svgWidth
        , transform <| "translate(0," ++ String.fromFloat yPos ++ ")"
        , fill stringColor
        ]
        []


singleNote : Model -> Notes.Note -> Int -> Int -> String -> Bool -> Svg msg
singleNote model note string fret class is_root =
    let
        w =
            if not is_root then
                model.coos.fretWidth fret * 0.8

            else
                model.coos.fretWidth fret * 0.8 - 6

        yPos =
            if not is_root then
                (S.convert model.coos.stringScale <| toFloat string) - 0.5 * model.coos.stringDistance

            else
                (S.convert model.coos.stringScale <| toFloat string) - 0.5 * model.coos.stringDistance + 3

        xPos =
            if not is_root then
                (S.convert model.coos.fretScale <| toFloat (fret - 1)) + (0.15 * model.coos.fretWidth fret)

            else
                (S.convert model.coos.fretScale <| toFloat (fret - 1)) + (0.15 * model.coos.fretWidth fret) + 3

        root_class =
            if is_root then
                [ Svg.Attributes.class "svg_rootnote" ]

            else
                []

        shorten t =
            if Tuple.first t == Tuple.second t then
                Tuple.first t

            else
                Tuple.first t ++ "/" ++ Tuple.second t

        y =
            String.fromFloat yPos

        x =
            String.fromFloat xPos

        h =
            if not is_root then
                model.coos.stringDistance

            else
                model.coos.stringDistance - 6

        textTranslateY =
            model.coos.stringDistance * 0.7

        textTranslateX =
            4

        --            model.coos.fretDistance * 0.5 - 30
        textTranslate =
            "translate(" ++ String.fromFloat textTranslateX ++ "," ++ String.fromFloat textTranslateY ++ ")"
    in
    Svg.g
        [ Svg.Attributes.transform <| "translate(" ++ x ++ "," ++ y ++ ")"
        ]
        [ Svg.rect
            (root_class
                ++ [ Svg.Attributes.class class
                   , Svg.Attributes.width <| String.fromFloat w
                   , Svg.Attributes.height <| String.fromFloat h

                   -- , Svg.Attributes.rx "5"
                   -- , Svg.Attributes.ry "5"
                   -- , Svg.Attributes.r <| String.fromFloat <| stringDistance * 0.4
                   ]
            )
            []
        , Svg.text_
            [ Svg.Attributes.transform textTranslate
            ]
            [ Svg.text <|
                if class /= "svg_nonote" then
                    shorten <| Notes.noteName note

                else
                    ""
            ]
        ]


getNoteCssClass : Notes.Note -> Notes.Note -> String
getNoteCssClass root n =
    let
        num =
            Notes.scaleDegreeAsInt root n + 1

        numString =
            String.fromInt num
    in
    "svg_note" ++ numString


drawScale : Model -> List (Svg msg)
drawScale model =
    let
        scale =
            Notes.makeScale model.root model.scale

        noteFill =
            \n ->
                case Dict.get (Notes.noteToInt n) model.selectedNotes of
                    Just True ->
                        getNoteCssClass model.root n

                    _ ->
                        "svg_nonote"

        notes =
            NeckNotes.notesOnString model.tuning model.frets

        notesOnSingleString stringNotes string =
            List.map (\( index, note ) -> singleNote model note string index (noteFill <| note) (note == model.root)) <| zip (List.range 0 model.frets) (List.map Notes.spnToPitch stringNotes)
    in
    List.concatMap (\( index, notes_ ) -> notesOnSingleString notes_ index) <| zip (List.range 1 6) notes


inlay : Model -> Int -> Svg msg
inlay model numFret =
    let
        x =
            (S.convert model.coos.fretScale <| toFloat numFret) - (0.5 * model.coos.fretWidth numFret)
    in
    Svg.circle
        [ Svg.Attributes.r <| String.fromFloat 7
        , Svg.Attributes.cx <| String.fromFloat x
        , Svg.Attributes.cy <| String.fromFloat -20
        , Svg.Attributes.fill "#999"
        ]
        []


doubleInlay : Model -> Int -> List (Svg msg)
doubleInlay model numFret =
    let
        x =
            (S.convert model.coos.fretScale <| toFloat numFret) - (0.5 * model.coos.fretWidth numFret)
    in
    [ Svg.circle
        [ Svg.Attributes.r <| String.fromFloat 7
        , Svg.Attributes.cx <| String.fromFloat x
        , Svg.Attributes.cy <| String.fromFloat -20
        , Svg.Attributes.fill "#999"
        ]
        []
    , Svg.circle
        [ Svg.Attributes.r <| String.fromFloat 7
        , Svg.Attributes.cx <| String.fromFloat x
        , Svg.Attributes.cy <| String.fromFloat -38
        , Svg.Attributes.fill "#999"
        ]
        []
    ]


fretBoard : Model.Model -> Html Msg
fretBoard model =
    let
        translateFretboard =
            transform <| "translate(" ++ String.fromFloat model.coos.translateX ++ "," ++ String.fromFloat model.coos.translateY ++ ")"
    in
    svg
        [ width <| String.fromFloat model.coos.svgWidth
        , height <| String.fromFloat model.coos.svgHeight
        ]
        [ g [ translateFretboard ]
            [ g [ id "fretBoard" ]
                [ rect
                    [ width <| String.fromFloat model.coos.svgWidth
                    , height <| String.fromFloat model.coos.neckHeight
                    , fill fretboardColor
                    ]
                    []
                ]
            , g [ id "frets" ] (List.map (singleFret model) <| List.range 0 model.frets)
            , g [ id "inlayDots" ]
                ([ inlay model 3
                 , inlay model 5
                 , inlay model 7
                 , inlay model 9
                 , inlay model 15
                 , inlay model 17
                 , inlay model 19
                 , inlay model 21
                 ]
                    ++ doubleInlay model 12
                    ++ doubleInlay model 24
                )
            , g [ id "strings" ] (List.map (singleString model) <| List.range 1 6)
            ]
        , g [ translateFretboard ]
            (drawScale model)
        ]
