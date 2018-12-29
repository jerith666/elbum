module Utils.TouchUtils exposing (Offset(..), TouchState, applyOffset, getOffset, init, update)

import Basics.Extra exposing (..)
import Html.Events.Extra.Touch exposing (..)
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Tuple exposing (..)


type Offset
    = NoOffset
    | Swipe Float
    | Zoom
        { scale : Float
        , startPos : ( Float, Float )
        , offset : ( Float, Float )
        }


type TouchState
    = NoState
    | SwipeState { start : Touch, current : Touch }
    | ZoomState
        { start : ( Touch, Touch )

        --, intermediates : List ( Touch, Touch )
        , current : ( Touch, Touch )
        }


init : TouchState
init =
    NoState


update : TouchState -> Event -> TouchState
update oldState touchEvent =
    case touchEvent.touches of
        [ t ] ->
            case oldState of
                SwipeState swipe ->
                    SwipeState { swipe | current = t }

                _ ->
                    SwipeState { start = t, current = t }

        [ t1, t2 ] ->
            case oldState of
                ZoomState zoom ->
                    ZoomState { zoom | current = ( t1, t2 ) }

                _ ->
                    ZoomState { start = ( t1, t2 ), current = ( t1, t2 ) }

        _ ->
            NoState


getOffset : TouchState -> Offset
getOffset state =
    case state of
        NoState ->
            NoOffset

        SwipeState swipe ->
            let
                ( x1, _ ) =
                    coords swipe.start

                ( x2, _ ) =
                    coords swipe.current
            in
            Swipe <| x2 - x1

        ZoomState zoom ->
            let
                startSize =
                    uncurry dist zoom.start

                endSize =
                    uncurry dist zoom.current

                scale =
                    endSize / startSize

                ( startX, startY ) =
                    uncurry center zoom.start

                ( endX, endY ) =
                    uncurry center zoom.current
            in
            Zoom
                { scale = scale
                , startPos = ( startX, startY )
                , offset = ( endX - startX, endY - startY )
                }


applyOffset : Offset -> ( Float, Float ) -> ( Float, Float )
applyOffset offset loc =
    case offset of
        Zoom z ->
            let
                ( locX, locY ) =
                    loc

                ( startX, startY ) =
                    z.startPos

                ( offX, offY ) =
                    z.offset

                --create AT that does:
                --1) translate startPos to origin
                --2) translate offset to origin
                --2) apply scale (dilate)
                --3) un-translate offset
                --4) un-translate startPos
                --then apply that AT to loc
                at =
                    Math.Matrix4.identity
                        |> translate3 -startX -startY 0
                        |> translate3 -offX -offY 0
                        |> scale3 z.scale z.scale z.scale
                        |> translate3 offX offY 0
                        |> translate3 startX startY 0

                newLoc =
                    transform at <| vec3 locX locY 0
            in
            ( getX newLoc, getY newLoc )

        _ ->
            loc


dist : Touch -> Touch -> Float
dist t1 t2 =
    let
        ( ( x1, y1 ), ( x2, y2 ) ) =
            ( coords t1, coords t2 )
    in
    sqrt <| (y2 - y1) ^ 2 + (x2 - x1) ^ 2


center : Touch -> Touch -> ( Float, Float )
center t1 t2 =
    let
        ( ( x1, y1 ), ( x2, y2 ) ) =
            ( coords t1, coords t2 )

        avg a b =
            (a + b) / 2
    in
    ( avg x1 x2, avg y1 y2 )


coords : Touch -> ( Float, Float )
coords t =
    let
        x =
            first t.clientPos

        y =
            second t.clientPos
    in
    ( x, y )
