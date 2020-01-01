module DrawingMath exposing (SvgCoordinates, calculate)

{-| Functions and types for calculating widths and ratios to dynamically scale
and display the Svg in width, while being able to scale the headstock
-}

import Browser.Dom exposing (Viewport)
import Scale as S


type alias SvgCoordinates =
    { svgWidth : Float
    , svgHeight : Float
    , translateX : Float
    , translateY : Float
    , fretScale : S.ContinuousScale Float
    , stringScale : S.ContinuousScale Float
    , fretDistance : Float
    , stringDistance : Float
    , fretWidth : Int -> Float
    , neckHeight : Float
    }


calculate : Maybe Viewport -> Int -> Float -> SvgCoordinates
calculate vp frets drawScalefactor =
    let
        svgWidth =
            case vp of
                Nothing ->
                    700.0

                Just v ->
                    v.viewport.width - 40

        svgHeight =
            neckHeight + 150

        translateX =
            0

        translateY =
            50

        fretDistance =
            (svgWidth - translateX) / toFloat (frets + 1)

        neckHeight =
            drawScalefactor * 50

        stringDistance =
            neckHeight / 6.0

        stringScale =
            S.linear ( neckHeight, 0 ) ( 0.5, 6.5 )

        fretScale =
            S.linear ( 0, svgWidth ) ( -1, toFloat frets )

        fretWidth =
            \n -> S.convert fretScale (toFloat n) - S.convert fretScale (toFloat n - 1)
    in
    { svgWidth = svgWidth
    , svgHeight = svgHeight
    , translateX = translateX
    , translateY = translateY
    , stringScale = stringScale
    , fretScale = fretScale
    , fretDistance = fretDistance
    , stringDistance = stringDistance
    , neckHeight = neckHeight
    , fretWidth = fretWidth
    }
