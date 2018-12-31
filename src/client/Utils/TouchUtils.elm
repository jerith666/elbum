module Utils.TouchUtils exposing (Direction(..), Offset(..), TouchState, ZoomOffset(..), applyOffset, cumScale, endZoom, getDirectionX, getOffset, init, update)

import Basics.Extra exposing (..)
import Html.Events.Extra.Touch exposing (..)
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import Tuple exposing (..)


type Offset
    = NoOffset
    | Swipe Float
    | Zoom ZoomOffset


type ZoomOffset
    = ZoomOffset
        { scale : Float
        , startPos : ( Float, Float )
        , offset : ( Float, Float )
        , prev : Maybe ZoomOffset
        }


type TouchState
    = NoState
    | SwipeState { start : Touch, current : Touch, prevZoom : Maybe ZoomData }
    | ZoomState ZoomData


type ZoomData
    = ZoomBridge ZoomData
    | ZoomCurrent ZoomCurrentData


type alias ZoomCurrentData =
    { prev : Maybe ZoomData
    , start : ( Touch, Touch )
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

                ZoomState zData ->
                    SwipeState { start = t, current = t, prevZoom = Just zData }

                NoState ->
                    SwipeState { start = t, current = t, prevZoom = Nothing }

        [ t1, t2 ] ->
            case oldState of
                ZoomState zoomData ->
                    case zoomData of
                        ZoomBridge prevData ->
                            ZoomState <|
                                ZoomCurrent
                                    { prev = Just prevData
                                    , start = ( t1, t2 )
                                    , current = ( t1, t2 )
                                    }

                        ZoomCurrent zData ->
                            ZoomState <| ZoomCurrent { zData | current = ( t1, t2 ) }

                SwipeState ss ->
                    case ss.prevZoom of
                        Nothing ->
                            update NoState touchEvent

                        Just oldZoom ->
                            ZoomState <|
                                ZoomCurrent
                                    { prev = Just oldZoom
                                    , start = ( t1, t2 )
                                    , current = ( t1, t2 )
                                    }

                NoState ->
                    ZoomState <|
                        ZoomCurrent
                            { prev = Nothing
                            , start = ( t1, t2 )
                            , current = ( t1, t2 )
                            }

        _ ->
            NoState


endZoom : TouchState -> TouchState
endZoom oldState =
    case oldState of
        NoState ->
            oldState

        SwipeState _ ->
            oldState

        ZoomState zoomData ->
            case zoomData of
                ZoomBridge bridgeData ->
                    ZoomState <| ZoomBridge bridgeData

                ZoomCurrent zData ->
                    case getZoomOffset zData of
                        ZoomOffset zo ->
                            case cumScale zo > 1 of
                                True ->
                                    ZoomState <| ZoomBridge <| ZoomCurrent zData

                                False ->
                                    NoState


cumScale z =
    case z.prev of
        Nothing ->
            z.scale

        Just (ZoomOffset pz) ->
            z.scale * cumScale pz


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

        ZoomState zData ->
            case zData of
                ZoomBridge oldZoom ->
                    getOffset <| ZoomState oldZoom

                ZoomCurrent zoomData ->
                    Zoom <| getZoomOffset zoomData


getZoomOffset : ZoomCurrentData -> ZoomOffset
getZoomOffset zoomData =
    let
        startSize =
            uncurry dist zoomData.start

        endSize =
            uncurry dist zoomData.current

        scale =
            endSize / startSize

        ( startX, startY ) =
            uncurry center zoomData.start

        ( endX, endY ) =
            uncurry center zoomData.current

        prevOffset =
            Maybe.map getZoomOffset <| Maybe.map getCurrentData zoomData.prev
    in
    ZoomOffset
        { scale = scale
        , startPos = ( startX, startY )
        , offset = ( endX - startX, endY - startY )
        , prev = prevOffset
        }


getCurrentData : ZoomData -> ZoomCurrentData
getCurrentData zoomData =
    case zoomData of
        ZoomBridge zData ->
            getCurrentData zData

        ZoomCurrent currentData ->
            currentData


applyOffset : ZoomOffset -> ( Float, Float ) -> ( Float, Float )
applyOffset (ZoomOffset z) origLoc =
    let
        loc =
            case z.prev of
                Nothing ->
                    origLoc

                Just pz ->
                    applyOffset pz origLoc

        ( locX, locY ) =
            loc

        ( startX, startY ) =
            z.startPos

        ( offX, offY ) =
            z.offset

        --create AT that does:
        --1) translate startPos to origin
        --2) apply scale (dilate)
        --4) translate offset
        --5) un-translate startPos
        --
        --note: we use (mul (makeXxx3 args)) rather than (xxx3 args) because
        --this transformation needs to go in the "less common" direction;
        --see https://docs.oracle.com/javase/10/docs/api/java/awt/geom/AffineTransform.html#preConcatenate(java.awt.geom.AffineTransform)
        at =
            Math.Matrix4.identity
                |> mul (makeTranslate3 -startX -startY 0)
                |> mul (makeScale3 z.scale z.scale 1)
                |> mul (makeTranslate3 offX offY 0)
                |> mul (makeTranslate3 startX startY 0)

        --then apply that AT to loc
        newLoc =
            transform at <| vec3 locX locY 0
    in
    ( getX newLoc, getY newLoc )


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


type Direction
    = Left
    | Right


getDirectionX : Float -> Direction
getDirectionX distance =
    case distance < 0 of
        True ->
            Left

        False ->
            Right
