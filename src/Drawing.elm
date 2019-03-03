module Drawing exposing (fretBoard, singleFret, singleString)

import HeadStock
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
        [ height "1.5"
        , width <| String.fromFloat svgWidth
        , transform <| "translate(0," ++ String.fromFloat yPos ++ ")"
        , fill string_color
        ]
        []


fretBoard : Model.Model -> Html Msg
fretBoard m =
    let
        svgWidth =
            case m.viewport of
                Nothing ->
                    700.0

                Just vp ->
                    vp.viewport.width

        svgHeight =
            m.drawScalefactor * HeadStock.headstockHeightUnscaled

        nFrets =
            m.frets

        translate_x =
            m.drawScalefactor * HeadStock.nutXUnscaled

        translate_y =
            m.drawScalefactor * HeadStock.nutYUnscaled

        fret_distance =
            (svgWidth - translate_x) / toFloat (nFrets - 1)

        fret_positions =
            List.map (\n -> toFloat n * fret_distance) <| List.range 0 (nFrets - 1)

        neck_height =
            m.drawScalefactor * HeadStock.nutHeightUnscaled
    in
    svg
        [ width <| String.fromFloat svgWidth
        , height <| String.fromFloat svgHeight
        ]
        [ g [ transform <| "translate(" ++ String.fromFloat translate_x ++ "," ++ String.fromFloat translate_y ++ ")" ]
            [ g [ id "fretBoard" ]
                [ rect
                    [ width <| String.fromFloat svgWidth
                    , height <| String.fromFloat neck_height
                    , fill fretboard_color
                    ]
                    []
                ]
            , g [] (List.map (\x -> singleFret x neck_height) fret_positions)
            , g [] (List.map (\x -> singleString (svgWidth - translate_x) neck_height x) <| List.range 1 6)
            ]
        , HeadStock.headStockGroup m.drawScalefactor
        ]
