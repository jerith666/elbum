module Sandbox.ScrollTest exposing (Model(..), Msg(..), main)

import AlbumStyles exposing (..)
import Browser exposing (Document)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import String exposing (fromFloat)


type Model
    = ScrollPos Float


type Msg
    = ScrolledTo Float


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( ScrollPos 0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update (ScrolledTo newScroll) (ScrollPos oldScroll) =
    ( ScrollPos newScroll, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "title"
    , body = [ viewImpl model |> toUnstyled ]
    }


viewImpl (ScrollPos s) =
    div
        [ on "scroll" <| Json.Decode.map ScrolledTo <| Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float
        , on "click" <| Json.Decode.succeed <| ScrolledTo 1
        , styles
            [ Css.height <| vh 100
            , Css.width <| vw 100
            , position absolute
            , overflow auto
            ]
        ]
    <|
        List.repeat
            100
        <|
            div []
                [ Html.Styled.text <|
                    "scroll at "
                        ++ fromFloat s
                ]
