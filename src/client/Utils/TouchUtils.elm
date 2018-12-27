module Utils.TouchUtils exposing (Offset(..), TouchState)

import Html.Events.Extra.Touch exposing (..)


type Offset
    = NoOffset
    | Swipe Float
    | Zoom { scale : Float, pos : ( Float, Float ) }


type TouchState
    = NoState
    | SwipeState { start : Touch, current : Touch }
    | ZoomState { starts : List ( Touch, Touch ), current : ( Touch, Touch ) }


init : TouchState
init =
    NoState


update : TouchState -> Event -> TouchState
update oldState touchEvent =
    oldState


getOffset : TouchState -> Offset
getOffset state =
    case state of
      NoState -> NoOffset
      SwipeState swipe ->
