module Sandbox.TouchTest exposing (main)

import Basics.Extra exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events.Extra.Touch exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)


type alias TouchTestModel =
    { touches : List Touch
    , zoomInfo : Maybe ZoomInfo
    }


type alias ZoomInfo =
    { zoomStart : ( Touch, Touch )
    , zoomCurrent : ( Touch, Touch )
    }


type TouchTestMsg
    = TouchMsg Event


main : Program () TouchTestModel TouchTestMsg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init _ =
    ( { touches = [], zoomInfo = Nothing }, Cmd.none )


update msg model =
    case msg of
        TouchMsg evt ->
            ( { model
                | touches = evt.touches
                , zoomInfo = updateZoomPair model.zoomInfo evt.touches
              }
            , Cmd.none
            )


updateZoomPair : Maybe ZoomInfo -> List Touch -> Maybe ZoomInfo
updateZoomPair existingZoom newTouches =
    case newTouches of
        [ t1, t2 ] ->
            case existingZoom of
                Just ez ->
                    Just { ez | zoomCurrent = ( t1, t2 ) }

                Nothing ->
                    Just { zoomStart = ( t1, t2 ), zoomCurrent = ( t1, t2 ) }

        _ ->
            Nothing


view model =
    div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "position" "absolute"
        , onStart TouchMsg
        , onMove TouchMsg
        , onEnd TouchMsg
        , onCancel TouchMsg
        ]
    <|
        drawTouches model
            :: div [] [ Html.text <| (String.fromInt <| List.length model.touches) ++ " touches active" ]
            :: List.map viewTouch model.touches


drawTouches : TouchTestModel -> Html TouchTestMsg
drawTouches model =
    svg
        [ Svg.Attributes.width "100vw"
        , Svg.Attributes.height "100vh"
        , Svg.Attributes.style "position: absolute"
        ]
    <|
        drawZoom model.zoomInfo
            :: List.concatMap drawTouch model.touches


drawZoom : Maybe ZoomInfo -> Svg TouchTestMsg
drawZoom zoomInfo =
    case zoomInfo of
        Nothing ->
            svg [] []

        Just zi ->
            let
                startCtr =
                    uncurry center zi.zoomStart

                curCtr =
                    uncurry center zi.zoomCurrent

                startSize =
                    uncurry dist zi.zoomStart

                curSize =
                    uncurry dist zi.zoomCurrent
            in
            svg []
                [ linePts startCtr curCtr "black"
                , circ startCtr startSize "black"
                , circ curCtr curSize "black"
                ]


circ ( x, y ) d color =
    circle
        [ cx <| String.fromFloat x
        , cy <| String.fromFloat y
        , r <| String.fromFloat <| d / 2
        , fill "none"
        , stroke color
        ]
        []


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


drawTouch touch =
    let
        x =
            first touch.clientPos

        y =
            second touch.clientPos
    in
    [ linePts ( x, 0 ) ( x, 10000 ) "black"
    , linePts ( 0, y ) ( 10000, y ) "black"
    ]


linePts : ( Float, Float ) -> ( Float, Float ) -> String -> Svg TouchTestMsg
linePts ( p1x, p1y ) ( p2x, p2y ) color =
    line
        [ x1 <| String.fromFloat p1x
        , y1 <| String.fromFloat p1y
        , x2 <| String.fromFloat p2x
        , y2 <| String.fromFloat p2y
        , stroke color
        ]
        []


viewTouch : Touch -> Html TouchTestMsg
viewTouch touch =
    div []
        [ Html.text <|
            "touch "
                ++ String.fromInt touch.identifier
                ++ " at ("
                ++ (String.fromFloat <| first touch.clientPos)
                ++ ", "
                ++ (String.fromFloat <| second touch.clientPos)
                ++ ")"
        ]


subscriptions _ =
    Sub.none
