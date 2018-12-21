module ScrollTest exposing (Model(..), Msg(..), main, update, view)

import AlbumStyles exposing (..)
import Css exposing (..)
import Html exposing (program)
import Html.Attributes exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode


type Model
    = ScrollPos Float


type Msg
    = ScrolledTo Float


main : Program Never Model Msg
main =
    Html.program
        { init = ( ScrollPos 0, Cmd.none )
        , update = update
        , view = view >> toUnstyled
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update (ScrolledTo newScroll) (ScrollPos oldScroll) =
    ( ScrollPos newScroll, Cmd.none )


view : Model -> Html Msg
view (ScrollPos s) =
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
                        ++ toString s
                ]
