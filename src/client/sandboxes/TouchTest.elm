module TouchTest exposing (main)

import AlbumListPage exposing (view)
import AlbumPage exposing (update)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra.Touch exposing (..)
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
        [ style "width" "100vw"
        , style "height" "100vh"
        , style "position" "absolute"
        , onStart TouchMsg
        , onMove TouchMsg
        , onEnd TouchMsg
        , onCancel TouchMsg
        ]
    <|
        div [] [ text <| (String.fromInt <| List.length model.touches) ++ " touches active" ]
            :: List.map viewTouch model.touches


viewTouch : Touch -> Html TouchTestMsg
viewTouch touch =
    div [] [ text <| "touch " ++ String.fromInt touch.identifier ++ " at (" ++ (String.fromFloat <| first touch.clientPos) ++ ", " ++ (String.fromFloat <| second touch.clientPos) ++ ")" ]


subscriptions model =
    Sub.none
