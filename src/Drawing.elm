module Drawing exposing (fretBoard, singleFret, singleString)

import HeadStock exposing (headStockGroup)
import Html exposing (..)
import Model exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


background_color =
    "#333333ff"


fretboard_color =
    "#321a0bff"


fret_color =
    "#ccccccff"


string_color =
    "#ffccaaff"


interval_ratio =
    1.059463


singleFret : Float -> Float -> Svg msg
singleFret fretX fretHeight =
    rect
        [ width "3"
        , height <| String.fromFloat fretHeight
        , transform <| "translate(" ++ String.fromFloat fretX ++ ",0)"
        , fill fret_color
        ]
        []


singleString : Float -> Float -> Int -> Svg msg
singleString svgWidth svgHeight nString =
    let
        stringDistance =
            svgHeight / 6.0

        yPos =
            (toFloat nString - 0.5) * stringDistance
    in
    rect
        [ height "2"
        , width <| String.fromFloat svgWidth
        , transform <| "translate(0," ++ String.fromFloat yPos ++ ")"
        , fill string_color
        ]
        []


fretBoard : Float -> Float -> Int -> Html Msg
fretBoard svgWidth svgHeight nFrets =
    let
        fret_distance =
            (svgWidth - 445) / toFloat (nFrets - 1)

        fret_positions =
            List.map (\n -> toFloat n * fret_distance) <| List.range 0 (nFrets - 1)
    in
    svg
        [ width <| String.fromFloat svgWidth
        , height <| String.fromFloat (svgHeight + 200)
        ]
        [ g [ transform "translate(445,87)" ]
            [ g [ id "fretBoard" ]
                [ rect
                    [ width <| String.fromFloat svgWidth
                    , height <| String.fromFloat svgHeight
                    , fill fretboard_color
                    ]
                    []
                ]
            , g [] (List.map (\x -> singleFret x svgHeight) fret_positions)
            , g [] (List.map (\x -> singleString (svgWidth - 445) svgHeight x) <| List.range 1 6)
            ]
        , headStockGroup
        ]
