module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onResize)
import Debug
import Dict
import Drawing exposing (fretBoard)
import DrawingMath
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (decodeString, int)
import List
import List.Extra exposing (zip, zip3)
import Model exposing (..)
import Notes
import Task



-- view model = svg [width "800", height "100"] []


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( startModel, Task.perform ViewportChange getViewport )



-- Update


toggle : Maybe Bool -> Maybe Bool
toggle =
    Maybe.andThen (\x -> Just <| not x)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewportChange newViewport ->
            let
                intermediateModel =
                    { model | viewport = Just newViewport, drawHeadstock = True }

                coos =
                    DrawingMath.calculate intermediateModel

                newModel =
                    DrawingMath.setHeadStockDraw intermediateModel
            in
            ( newModel, Cmd.none )

        WindowResize ->
            ( model, Task.perform ViewportChange getViewport )

        NumFretsInc ->
            if model.frets < 24 then
                ( DrawingMath.setHeadStockDraw { model | frets = model.frets + 1 }, Cmd.none )

            else
                ( DrawingMath.setHeadStockDraw model, Cmd.none )

        NumFretsDec ->
            if model.frets > 5 then
                ( DrawingMath.setHeadStockDraw { model | frets = model.frets - 1 }, Cmd.none )

            else
                ( DrawingMath.setHeadStockDraw model, Cmd.none )

        ScaleSelected s ->
            case s of
                "major" ->
                    ( Model.switchScale model Notes.majorScale, Cmd.none )

                "majorPentatonic" ->
                    ( Model.switchScale model Notes.majorPentatonicScale, Cmd.none )

                "minor" ->
                    ( Model.switchScale model Notes.minorScale, Cmd.none )

                "minorPentatonic" ->
                    ( Model.switchScale model Notes.minorPentatonicScale, Cmd.none )

                "blues" ->
                    ( Model.switchScale model Notes.bluesScale, Cmd.none )

                _ ->
                    ( Model.switchScale model Notes.majorScale, Cmd.none )

        RootSelected r ->
            case decodeString int r of
                Result.Ok n ->
                    ( Model.switchRoot model <| Notes.intToNote n, Cmd.none )

                Result.Err _ ->
                    ( model, Cmd.none )

        NoteSelection s ->
            case decodeString int s of
                Result.Ok n ->
                    ( { model | selectedNotes = Dict.update n toggle model.selectedNotes }, Cmd.none )

                Result.Err _ ->
                    ( model, Cmd.none )



-- View


getNoteCssClass : Notes.Note -> String
getNoteCssClass n =
    let
        num =
            Notes.noteToInt n + 1

        numString =
            String.fromInt num
    in
    "note" ++ numString


scaleDisplay : Model -> List (Html Msg)
scaleDisplay m =
    let
        notes =
            List.map Notes.intToNote <|
                List.map (modBy 12) <|
                    List.range (0 + Notes.noteToInt m.root)
                        (11 + Notes.noteToInt m.root)

        scale =
            Notes.makeScale m.root m.scale

        names =
            List.map Notes.noteName notes

        classes =
            List.map
                (\n ->
                    if List.member n scale then
                        getNoteCssClass n

                    else
                        "nonote"
                )
                notes

        shorten t =
            if Tuple.first t == Tuple.second t then
                Tuple.first t

            else
                Tuple.first t ++ "/" ++ Tuple.second t

        shortnames =
            List.map shorten names

        notesClassesNames =
            zip3 notes classes shortnames

        toDiv ( note, class_, name ) =
            div [ class class_ ]
                (if class_ /= "nonote" then
                    [ Html.input
                        [ type_ "checkbox"
                        , value (String.fromInt <| Notes.noteToInt <| note)
                        , checked
                            (case
                                Dict.get (Notes.noteToInt note) m.selectedNotes
                             of
                                Just b ->
                                    b

                                Nothing ->
                                    False
                            )
                        , onCheck (\b -> NoteSelection (String.fromInt <| Notes.noteToInt <| note))
                        ]
                        []
                    , text (" " ++ name)
                    ]

                 else
                    [ text name
                    ]
                )
    in
    List.map toDiv notesClassesNames


noteOptions : List (Html Msg)
noteOptions =
    let
        notes =
            List.map Notes.intToNote (List.range 0 11)

        names =
            List.map Notes.noteName notes

        shorten t =
            if Tuple.first t == Tuple.second t then
                Tuple.first t

            else
                Tuple.first t ++ "/" ++ Tuple.second t

        shortnames =
            List.map shorten names

        numberedShortnames =
            zip (List.range 0 11) shortnames

        toOption ( index, name ) =
            option [ value <| String.fromInt index ] [ text name ]
    in
    List.map toOption numberedShortnames


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
    [ div []
        [ h1 []
            [ Html.text "Banana for Scale" ]
        ]
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
                [ text "Root:  " ]
            , select [ id "rootselect", onInput RootSelected ] noteOptions
            ]
        , div
            [ class "setting" ]
            [ span
                []
                [ text "Scale:  " ]
            , select [ id "scaleselect", onInput ScaleSelected ]
                [ option [ value "major" ] [ text "Major" ]
                , option [ value "majorPentatonic" ] [ text "Major Pentatonic" ]
                , option [ value "minor" ] [ text "Minor" ]
                , option [ value "minorPentatonic" ] [ text "Minor Pentatonic" ]
                , option [ value "blues" ] [ text "Blues" ]
                ]
            ]
        ]
    , div
        [ id "scale" ]
        (scaleDisplay m)
    , div [ id "content" ]
        [ fretBoard m
        ]
    , div [ id "footer" ]
        [ div []
            [ Html.text "Source "
            , a [ href "https://github.com/brtmr/bananaforscale" ] [ Html.text "here" ]
            ]
        , a
            [ href "https://github.com/brtmr/bananaforscale" ]
            [ Html.img [ src "banana.png", height 50 ] [] ]
        , div []
            [ Html.text "Made with "
            , a [ href "https://elm-lang.org" ] [ Html.text "elm" ]
            ]
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
