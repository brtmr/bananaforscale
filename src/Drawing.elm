module Drawing exposing (fretBoard, getNoteCssClass, singleFret, singleString)

import Dict exposing (Dict)
import DrawingMath
import HeadStock
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
    "#321a0bff"


fretColor =
    "#ccccccff"


stringColor =
    "#ffccaaff"


intervalRatio =
    1.059463


singleFret : DrawingMath.SvgCoordinates -> Float -> Int -> Svg msg
singleFret coos fretX fretNum =
    let
        halfFretBack =
            String.fromFloat <| (coos.fretDistance / -2.0) - 10

        neckHeight =
            String.fromFloat <| coos.neckHeight

        textHeight =
            String.fromFloat <| coos.neckHeight + 35
    in
    Svg.g
        [ transform <| "translate(" ++ String.fromFloat fretX ++ ",0)"
        ]
        [ rect
            [ width "3"
            , height neckHeight
            , fill fretColor
            ]
            []
        , Svg.text_
            [ Svg.Attributes.transform <| "translate(" ++ halfFretBack ++ ", " ++ textHeight ++ ")"
            , Svg.Attributes.fill "#666"
            ]
            [ Svg.text <| String.fromInt fretNum ]
        ]


singleString : Model -> Int -> Svg msg
singleString model nString =
    let
        yPos =
            S.convert model.coos.stringScale (toFloat nString)
    in
    rect
        [ height "1.5"
        , width <| String.fromFloat model.coos.svgWidth
        , transform <| "translate(0," ++ String.fromFloat yPos ++ ")"
        , fill stringColor
        ]
        []


singleNote : Model -> Notes.Note -> Int -> Int -> String -> Bool -> Svg msg
singleNote model note string fret class is_root =
    let
        w =
            model.coos.fretWidth fret * 0.8

        yPos =
            (S.convert model.coos.stringScale <| toFloat string) - 0.4 * model.coos.stringDistance

        xPos =
            (S.convert model.coos.fretScale <| toFloat (fret - 1)) + (0.2 * w)

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
            model.coos.stringDistance * 0.8

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
                   , Svg.Attributes.rx "5"
                   , Svg.Attributes.ry "5"

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


inlay : Float -> Float -> Int -> Svg msg
inlay fretDistance neckHeight numFret =
    Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (neckHeight * 0.04)
        , Svg.Attributes.cx <| String.fromFloat (((numFret |> toFloat) - 0.5) * fretDistance)
        , Svg.Attributes.cy <| String.fromFloat (-0.2 * neckHeight)
        , Svg.Attributes.fill "#666"
        ]
        []


doubleInlay : Float -> Float -> Int -> List (Svg msg)
doubleInlay fretDistance neckHeight numFret =
    [ Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (neckHeight * 0.04)
        , Svg.Attributes.cx <| String.fromFloat (((numFret |> toFloat) - 0.5) * fretDistance)
        , Svg.Attributes.cy <| String.fromFloat (-0.2 * neckHeight)
        , Svg.Attributes.fill "#666"
        ]
        []
    , Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (neckHeight * 0.04)
        , Svg.Attributes.cx <| String.fromFloat (((numFret |> toFloat) - 0.5) * fretDistance)
        , Svg.Attributes.cy <| String.fromFloat (-0.33 * neckHeight)
        , Svg.Attributes.fill "#666"
        ]
        []
    ]


fretBoard : Model.Model -> Html Msg
fretBoard model =
    let
        numberedFrets =
            zip (List.range 0 model.frets) (List.map (toFloat >> S.convert model.coos.fretScale) (List.range 0 model.frets))

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
            , g [ id "frets" ] (List.map (\( n, pos ) -> singleFret model.coos pos n) numberedFrets)
            , g [ id "inlayDots" ]
                ([ inlay model.coos.fretDistance model.coos.neckHeight 3
                 , inlay model.coos.fretDistance model.coos.neckHeight 5
                 , inlay model.coos.fretDistance model.coos.neckHeight 7
                 , inlay model.coos.fretDistance model.coos.neckHeight 9
                 , inlay model.coos.fretDistance model.coos.neckHeight 15
                 , inlay model.coos.fretDistance model.coos.neckHeight 17
                 , inlay model.coos.fretDistance model.coos.neckHeight 19
                 , inlay model.coos.fretDistance model.coos.neckHeight 21
                 ]
                    ++ doubleInlay model.coos.fretDistance model.coos.neckHeight 12
                    ++ doubleInlay model.coos.fretDistance model.coos.neckHeight 24
                )
            , g [ id "strings" ] (List.map (singleString model) <| List.range 1 6)
            ]
        , if model.drawHeadstock then
            HeadStock.headStockGroup model.drawScalefactor

          else
            Svg.g [] []
        , g [ translateFretboard ]
            (drawScale model)
        ]
