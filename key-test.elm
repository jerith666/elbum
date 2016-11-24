module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Keyboard exposing (..)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Int


type Msg
    = KeyPress Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress k ->
            ( k, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


view : Model -> Html Msg
view model =
    h1
        [ onMouseEnter (KeyPress 1337)
        , onMouseLeave (KeyPress 1338)
        ]
        [ text ("key pressed: " ++ (toString model)) ]


subscriptions model =
    presses KeyPress
