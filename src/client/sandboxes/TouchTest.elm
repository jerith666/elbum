module TouchTest exposing (main)

import AlbumListPage exposing (view)
import AlbumPage exposing (update)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra.Touch exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)


type alias TouchTestModel =
    { touches : List Touch }


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
    ( { touches = [] }, Cmd.none )


update msg model =
    case msg of
        TouchMsg evt ->
            ( { model | touches = evt.touches }, Cmd.none )


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
        drawTouches model.touches
            :: div [] [ Html.text <| (String.fromInt <| List.length model.touches) ++ " touches active" ]
            :: List.map viewTouch model.touches


drawTouches : List Touch -> Html TouchTestMsg
drawTouches touches =
    svg
        [ Svg.Attributes.width "100vw"
        , Svg.Attributes.height "100vh"
        , Svg.Attributes.style "position: absolute"
        ]
    <|
        List.concatMap drawTouch touches


drawTouch touch =
    let
        x =
            String.fromFloat <| first touch.clientPos

        y =
            String.fromFloat <| second touch.clientPos
    in
    [ line [ x1 x, x2 x, y1 "0", y2 "10000", stroke "black" ] []
    , line [ x1 "0", x2 "10000", y1 y, y2 y, stroke "black" ] []
    ]


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


subscriptions model =
    Sub.none
