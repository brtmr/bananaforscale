module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Drawing exposing (fretBoard)
import Html exposing (..)
import List
import Model exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
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
            ( { model | viewport = Just newViewport }, Cmd.none )

        WindowResize ->
            ( model, Task.perform ViewportChange getViewport )



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
        [ select [ id "scale_select" ]
            []
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
