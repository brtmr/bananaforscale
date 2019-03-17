module DrawingMath exposing (SvgCoordinates, calculate, setHeadStockDraw)

{-| Functions and types for calculating widths and ratios to dynamically scale
and display the Svg in width, while being able to scale the headstock
-}

import HeadStock
import Model exposing (Model)


type alias SvgCoordinates =
    { svgWidth : Float
    , svgHeight : Float
    , translateX : Float
    , translateY : Float
    , fretDistance : Float
    , fretPositions : List Float
    , neckHeight : Float
    }


calculate : Model -> SvgCoordinates
calculate m =
    let
        svgWidth =
            case m.viewport of
                Nothing ->
                    700.0

                Just vp ->
                    vp.viewport.width - 40

        svgHeight =
            m.drawScalefactor * HeadStock.headstockHeightUnscaled

        translateX =
            if m.drawHeadstock then
                m.drawScalefactor * HeadStock.nutXUnscaled

            else
                50.0

        translateY =
            m.drawScalefactor * HeadStock.nutYUnscaled

        fretDistance =
            (svgWidth - translateX) / toFloat (m.frets + 1)

        fretPositions =
            List.map (\n -> toFloat n * fretDistance) <| List.range 0 m.frets

        neckHeight =
            m.drawScalefactor * HeadStock.nutHeightUnscaled
    in
    { svgWidth = svgWidth
    , svgHeight = svgHeight
    , translateX = translateX
    , translateY = translateY
    , fretDistance = fretDistance
    , fretPositions = fretPositions
    , neckHeight = neckHeight
    }


setHeadStockDraw : Model -> Model
setHeadStockDraw model =
    let
        intermediateModel =
            { model | drawHeadstock = True }

        coos =
            calculate intermediateModel

        newModel =
            if Debug.log "d = " coos.fretDistance > 60 then
                { intermediateModel | drawHeadstock = True }

            else
                { intermediateModel | drawHeadstock = False }
    in
    newModel
