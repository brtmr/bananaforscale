module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Drawing exposing (fretBoard)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List
import Model exposing (..)
import Notes
import Task



-- view model = svg [width "800", height "100"] []


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( startModel, Task.perform ViewportChange getViewport )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewportChange newViewport ->
            if newViewport.viewport.width > 900 then
                ( { model | viewport = Just newViewport, drawHeadstock = True }, Cmd.none )

            else
                ( { model | viewport = Just newViewport, drawHeadstock = False }, Cmd.none )

        WindowResize ->
            ( model, Task.perform ViewportChange getViewport )

        NumFretsInc ->
            if model.frets < 24 then
                ( { model | frets = model.frets + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        NumFretsDec ->
            if model.frets > 5 then
                ( { model | frets = model.frets - 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        ScaleSelected s ->
            case s of
                "major" ->
                    ( { model | scale = Notes.majorScale }, Cmd.none )

                "minor" ->
                    ( { model | scale = Notes.minorScale }, Cmd.none )

                _ ->
                    ( { model | scale = Notes.majorScale }, Cmd.none )



-- View


body : Model -> List (Html Msg)
body m =
    let
        vpWidth =
            case m.viewport of
                Nothing ->
                    700.0

                Just vp ->
                    vp.viewport.width

        svgWidth =
            vpWidth * 0.99

        svgHeight =
            150.0
    in
    [ h1 []
        [ Html.text "Banana for scale" ]
    , div
        [ id "settings" ]
        [ div
            [ class "setting" ]
            [ span
                []
                [ Html.text <| String.fromInt m.frets ++ " Frets" ]
            , button [ onClick Model.NumFretsDec ]
                [ Html.text "-" ]
            , button
                [ onClick Model.NumFretsInc ]
                [ Html.text "+" ]
            ]
        , div
            [ class "setting" ]
            [ span
                []
                [ text "Scale:  " ]
            , select [ id "scaleselect", onInput ScaleSelected ]
                [ option [ value "major" ] [ text "Major" ]
                , option [ value "minor" ] [ text "Minor" ]
                ]
            ]
        ]
    , div []
        [ fretBoard m
        ]
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Banana for Scale", body = body model }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\_ _ -> WindowResize)



-- Application


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
