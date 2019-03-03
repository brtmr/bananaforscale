module Drawing exposing (fretBoard, singleFret, singleString)

import HeadStock
import Html exposing (..)
import List.Extra exposing (zip)
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


singleFret : Float -> Float -> Int -> Svg msg
singleFret fretX fretHeight fretNum =
    Svg.g
        [ transform <| "translate(" ++ String.fromFloat fretX ++ ",0)"
        ]
        [ rect
            [ width "3"
            , height <| String.fromFloat fretHeight
            , fill fret_color
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
        , fill string_color
        ]
        []


inlay : Float -> Float -> Int -> Svg msg
inlay fret_distance neck_height num_fret =
    Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (fret_distance * 0.1)
        , Svg.Attributes.cx <| String.fromFloat (((num_fret |> toFloat) - 0.5) * fret_distance)
        , Svg.Attributes.cy <| String.fromFloat (0.5 * neck_height)
        , Svg.Attributes.fill fret_color
        ]
        []


double_inlay : Float -> Float -> Int -> List (Svg msg)
double_inlay fret_distance neck_height num_fret =
    [ Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (fret_distance * 0.1)
        , Svg.Attributes.cx <| String.fromFloat (((num_fret |> toFloat) - 0.5) * fret_distance)
        , Svg.Attributes.cy <| String.fromFloat (0.25 * neck_height)
        , Svg.Attributes.fill fret_color
        ]
        []
    , Svg.circle
        [ Svg.Attributes.r <| String.fromFloat (fret_distance * 0.1)
        , Svg.Attributes.cx <| String.fromFloat (((num_fret |> toFloat) - 0.5) * fret_distance)
        , Svg.Attributes.cy <| String.fromFloat (0.75 * neck_height)
        , Svg.Attributes.fill fret_color
        ]
        []
    ]


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
            (svgWidth - translate_x) / toFloat (nFrets + 1)

        fret_positions =
            List.map (\n -> toFloat n * fret_distance) <| List.range 0 nFrets

        numbered_frets =
            zip (List.range 0 nFrets) fret_positions

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
            , g [ id "frets" ] (List.map (\( n, pos ) -> singleFret pos neck_height n) numbered_frets)
            , g [ id "inlay_dots" ]
                ([ inlay fret_distance neck_height 3
                 , inlay fret_distance neck_height 5
                 , inlay fret_distance neck_height 7
                 , inlay fret_distance neck_height 9
                 ]
                    ++ double_inlay fret_distance neck_height 12
                )
            , g [ id "strings" ] (List.map (\x -> singleString (svgWidth - translate_x) neck_height x) <| List.range 1 6)
            ]
        , HeadStock.headStockGroup m.drawScalefactor
        ]
