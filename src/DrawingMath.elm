module DrawingMath exposing (SvgCoordinates, calculate)

{-| Functions and types for calculating widths and ratios to dynamically scale
and display the Svg in width, while being able to scale the headstock
-}

import Browser.Dom exposing (Viewport)
import HeadStock
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


calculate : Maybe Viewport -> Int -> Float -> Bool -> SvgCoordinates
calculate vp frets drawScalefactor drawHeadstock =
    let
        svgWidth =
            case vp of
                Nothing ->
                    700.0

                Just v ->
                    v.viewport.width - 40

        svgHeight =
            drawScalefactor * HeadStock.headstockHeightUnscaled

        translateX =
            if drawHeadstock then
                drawScalefactor * HeadStock.nutXUnscaled

            else
                50.0

        translateY =
            drawScalefactor * HeadStock.nutYUnscaled

        fretDistance =
            (svgWidth - translateX) / toFloat (frets + 1)

        neckHeight =
            drawScalefactor * HeadStock.nutHeightUnscaled

        stringDistance =
            neckHeight / 6.0

        stringScale =
            S.linear ( neckHeight, 0 ) ( 0.5, 6.5 )

        fretScale =
            S.linear ( 0, fretDistance * toFloat (frets + 1) ) ( 0, toFloat frets )
    in
    { svgWidth = svgWidth
    , svgHeight = svgHeight
    , translateX = translateX
    , translateY = translateY
    , stringScale = stringScale
    , fretScale = fretScale
    , fretDistance = fretDistance
    , fretWidth = \n -> S.convert fretScale (toFloat n) - S.convert fretScale (toFloat n - 1)
    , stringDistance = stringDistance
    , neckHeight = neckHeight
    }
