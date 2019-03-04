module Drawing exposing (fretBoard, singleFret, singleString)

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


noteColors : List String
noteColors =
    [ "#fbb4ae"
    , "#b3cde3"
    , "#ccebc5"
    , "#decbe4"
    , "#fed9a6"
    , "#ffffcc"
    , "#e5d8bd"
    , "#fddaec"
    ]


singleFret : Float -> Float -> Int -> Svg msg
singleFret fretX fretHeight fretNum =
    Svg.g
        [ transform <| "translate(" ++ String.fromFloat fretX ++ ",0)"
        ]
        [ rect
            [ width "3"
            , height <| String.fromFloat fretHeight
            , fill fretColor
            ]
            []
        , Svg.text_
            [ Svg.Attributes.transform "translate(-5, -10)"
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


singleNote : Model -> Int -> Int -> String -> Svg msg
singleNote model string fret fill =
    let
        coos =
            DrawingMath.calculate model

        stringDistance =
            coos.neckHeight / 6.0

        yPos =
            (toFloat string - 0.5) * stringDistance

        xPos =
            (toFloat fret - 0.5) * coos.fretDistance
    in
    Svg.circle
        [ Svg.Attributes.fill fill
        , Svg.Attributes.cy <| String.fromFloat yPos
        , Svg.Attributes.cx <| String.fromFloat xPos
        , Svg.Attributes.r <| String.fromFloat <| stringDistance * 0.4
        ]
        []


drawScale : Model -> List (Svg msg)
drawScale model =
    let
        scale =
            Notes.makeScale model.root model.scale

        coloredNotes =
            Dict.fromList <| zip (List.map Notes.noteToInt scale) noteColors

        noteFill =
            \n ->
                case Dict.get (Notes.noteToInt n) coloredNotes of
                    Just color ->
                        color

                    Nothing ->
                        "none"

        notes =
            NeckNotes.notesOnString model.tuning model.frets

        spnPitch spn =
            case spn of
                Notes.SPN pitch _ ->
                    pitch

        notesOnSingleString stringNotes string =
            List.map (\( index, note ) -> singleNote model string index (noteFill <| spnPitch note)) <| zip (List.range 0 model.frets) stringNotes
    in
    List.concatMap (\( index, notes_ ) -> notesOnSingleString notes_ index) <| zip (List.range 1 6) notes


inlay : Float -> Float -> Int -> Svg msg
inlay fretDistance neckHeight numFret =
    Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (neckHeight * 0.05)
        , Svg.Attributes.cx <| String.fromFloat (((numFret |> toFloat) - 0.5) * fretDistance)
        , Svg.Attributes.cy <| String.fromFloat (0.5 * neckHeight)
        , Svg.Attributes.fill fretColor
        ]
        []


doubleInlay : Float -> Float -> Int -> List (Svg msg)
doubleInlay fretDistance neckHeight numFret =
    [ Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (neckHeight * 0.05)
        , Svg.Attributes.cx <| String.fromFloat (((numFret |> toFloat) - 0.5) * fretDistance)
        , Svg.Attributes.cy <| String.fromFloat (0.25 * neckHeight)
        , Svg.Attributes.fill fretColor
        ]
        []
    , Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (neckHeight * 0.05)
        , Svg.Attributes.cx <| String.fromFloat (((numFret |> toFloat) - 0.5) * fretDistance)
        , Svg.Attributes.cy <| String.fromFloat (0.75 * neckHeight)
        , Svg.Attributes.fill fretColor
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
            , g [ id "frets" ] (List.map (\( n, pos ) -> singleFret pos coos.neckHeight n) numberedFrets)
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
