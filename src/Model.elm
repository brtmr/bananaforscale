module Model exposing (Flags, Model, Msg(..))

import Browser.Dom exposing (Viewport)


type alias Model =
    { viewport : Maybe Viewport }


type Msg
    = ViewportChange Viewport
    | WindowResize


type alias Flags =
    ()
