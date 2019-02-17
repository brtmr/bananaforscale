import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser
import Browser.Events exposing (onResize)
import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (..)
import Task



-- view model = svg [width "800", height "100"] []



-- Model

type alias Model = 
    { viewport : Maybe Viewport }

type Msg 
    = ViewportChange Viewport
    | WindowResize

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init _ = 
    (Model Nothing, Task.perform ViewportChange getViewport)

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        ViewportChange newViewport ->
            ({ model | viewport = Just newViewport}, Cmd.none)
        WindowResize -> 
            (model, (Task.perform ViewportChange getViewport))

-- View

body : Model -> List (Html Msg)
body m = 
    let 
        vpWidth =
            case m.viewport of 
                Nothing -> 700.0
                Just vp -> vp.viewport.width
        svgWidth = vpWidth * 0.8
        background_color = "#333333ff"
        fretboard_color = "#321a0bff"
        fret_color = "#ccccccff"
        string_color = "#ffccaaff"
        interval_ratio = 1.059463
    in
        [ div [] 
            [
                svg [width <| String.fromFloat svgWidth, height "300"] [
                g [] 
                    [
                        rect 
                            [ width <| String.fromFloat svgWidth
                            , height "300"
                            , fill fretboard_color
                            ] []
                    ]    
                ]
            ] 
        ]

view : Model -> Browser.Document Msg
view model = { title = "Banana for Scale" , body = body model }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = 
    onResize (\_ _ -> WindowResize)

-- Application

main = Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
