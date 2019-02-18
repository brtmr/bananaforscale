import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser
import Browser.Events exposing (onResize)
import Browser.Dom exposing (Viewport, getViewport)
import Html exposing (..)
import Task
import List


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


background_color = "#333333ff"
fretboard_color = "#321a0bff"
fret_color = "#ccccccff"
string_color = "#ffccaaff"
interval_ratio = 1.059463

singleFret : Float -> Float -> Svg Msg
singleFret fretX fretHeight = rect 
    [ width "3"
    , height <| String.fromFloat fretHeight
    , transform <| "translate(" ++ (String.fromFloat fretX) ++ ",0)"
    , fill fret_color
    ] 
    []

singleString : Float -> Float -> Int -> Svg Msg
singleString svgWidth svgHeight nString = 
    let 
        stringDistance = svgHeight / 6.0
        yPos = ((toFloat nString) - 0.5) * stringDistance
    in rect 
        [ height "2"
        , width <| String.fromFloat svgWidth
        , transform <| "translate(0," ++ (String.fromFloat yPos) ++ ")"
        , fill string_color
        ] []


fretBoard : Float -> Float -> Int -> Html Msg
fretBoard svgWidth svgHeight nFrets = 
    let 
        fret_distance = svgWidth / (toFloat (nFrets - 1))
        fret_positions = List.map (\n -> (toFloat n) * fret_distance) <| List.range 0 (nFrets-1)
    in
    svg [ width <| String.fromFloat svgWidth 
        , height <| String.fromFloat svgHeight
        ] 
        [ g [ id "fretBoard" ] 
            [
                rect 
                    [ width <| String.fromFloat svgWidth
                    , height <| String.fromFloat svgHeight
                    , fill fretboard_color
                    ] []
            ]    
        , g [] (List.map (\x -> singleFret x svgHeight) fret_positions)
        , g [] (List.map (\x -> singleString svgWidth svgHeight x) <| List.range 1 6 )
        ]

body : Model -> List (Html Msg)
body m = 
    let 
        vpWidth =
            case m.viewport of 
                Nothing -> 700.0
                Just vp -> vp.viewport.width
        svgWidth = vpWidth * 0.8
        svgHeight = 100.0
    in
        [ div [] 
            [
                (fretBoard svgWidth svgHeight 22)
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
