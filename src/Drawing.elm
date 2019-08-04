module Drawing exposing (fretBoard, getNoteCssClass, singleFret, singleString)

import Dict exposing (Dict)
import DrawingMath
import HeadStock
import Html exposing (..)
import List.Extra exposing (zip)
import Model exposing (..)
import NeckNotes
import Notes
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
            String.fromFloat <| coos.neckHeight + 35
    in
    Svg.g
        [ transform <| "translate(" ++ String.fromFloat fretX ++ ",0)"
        ]
        [ rect
            [ width "3"
            , height <| String.fromFloat coos.neckHeight
            , fill fretColor
            ]
            []
        , Svg.text_
            [ Svg.Attributes.transform <| "translate(" ++ halfFretBack ++ ", " ++ neckHeight ++ ")"
            , Svg.Attributes.fill "#666"
            ]
            [ Svg.text <| String.fromInt fretNum ]
        ]


singleString : Float -> Float -> Int -> Svg msg
singleString svgWidth svgHeight nString =
    let
        stringDistance =
            svgHeight / 6.0

        yPos =
            (toFloat nString - 0.5) * stringDistance
    in
    rect
        [ height "1.5"
        , width <| String.fromFloat svgWidth
        , transform <| "translate(0," ++ String.fromFloat yPos ++ ")"
        , fill stringColor
        ]
        []


singleNote : Model -> Int -> Int -> String -> Bool -> Svg msg
singleNote model string fret class is_root =
    let
        coos =
            DrawingMath.calculate model

        stringDistance =
            coos.neckHeight / 6.0

        yPos =
            (toFloat (6 - string) + 0.5) * stringDistance

        xPos =
            if fret > 0 then
                (toFloat fret - 0.5) * coos.fretDistance

            else
                -25

        root_class =
            if is_root then
                [ Svg.Attributes.class "svg_rootnote" ]

            else
                []
    in
    Svg.circle
        (root_class
            ++ [ Svg.Attributes.class class
               , Svg.Attributes.cy <| String.fromFloat yPos
               , Svg.Attributes.cx <| String.fromFloat xPos
               , Svg.Attributes.r <| String.fromFloat <| stringDistance * 0.4
               ]
        )
        []


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
            List.map (\( index, note ) -> singleNote model string index (noteFill <| note) (note == model.root)) <| zip (List.range 0 model.frets) (List.map Notes.spnToPitch stringNotes)
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
fretBoard m =
    let
        coos =
            DrawingMath.calculate m

        numberedFrets =
            zip (List.range 0 m.frets) coos.fretPositions

        translateFretboard =
            transform <| "translate(" ++ String.fromFloat coos.translateX ++ "," ++ String.fromFloat coos.translateY ++ ")"
    in
    svg
        [ width <| String.fromFloat coos.svgWidth
        , height <| String.fromFloat coos.svgHeight
        ]
        [ g [ translateFretboard ]
            [ g [ id "fretBoard" ]
                [ rect
                    [ width <| String.fromFloat coos.svgWidth
                    , height <| String.fromFloat coos.neckHeight
                    , fill fretboardColor
                    ]
                    []
                ]
            , g [ id "frets" ] (List.map (\( n, pos ) -> singleFret coos pos n) numberedFrets)
            , g [ id "inlayDots" ]
                ([ inlay coos.fretDistance coos.neckHeight 3
                 , inlay coos.fretDistance coos.neckHeight 5
                 , inlay coos.fretDistance coos.neckHeight 7
                 , inlay coos.fretDistance coos.neckHeight 9
                 , inlay coos.fretDistance coos.neckHeight 15
                 , inlay coos.fretDistance coos.neckHeight 17
                 , inlay coos.fretDistance coos.neckHeight 19
                 , inlay coos.fretDistance coos.neckHeight 21
                 ]
                    ++ doubleInlay coos.fretDistance coos.neckHeight 12
                    ++ doubleInlay coos.fretDistance coos.neckHeight 24
                )
            , g [ id "strings" ] (List.map (\x -> singleString (coos.svgWidth - coos.translateX) coos.neckHeight x) <| List.range 1 6)
            ]
        , if m.drawHeadstock then
            HeadStock.headStockGroup m.drawScalefactor

          else
            Svg.g [] []
        , g [ translateFretboard ]
            (drawScale m)
        ]
