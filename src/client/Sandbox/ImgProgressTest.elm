module Sandbox.ImgProgressTest exposing (Model(..), Msg(..), cached, fallback, init, main, mainImg, subscriptions, update, view)

--import Css exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (..)
--import Json.Decode exposing (..)

import Album exposing (..)
import Html exposing (program)
import Html.Styled exposing (..)
import ProgressiveImage exposing (..)


type Model
    = Mo ProgressiveImageModel


type Msg
    = Me ProgressiveImageMsg


main : Program Never Model Msg
main =
    Html.program { init = init, view = view >> toUnstyled, update = update, subscriptions = subscriptions }


mainImg : ImgSrc
mainImg =
    ImgSrc "10_October 06-09: Ohiopyle Weekend/DSC_1495.JPG" 1600 1069


fallback : ImgSrc
fallback =
    ImgSrc "10_October 06-09: Ohiopyle Weekend/DSC_1495.200.png" 200 133


cached : ImgSrc
cached =
    ImgSrc "10_October 06-09: Ohiopyle Weekend/DSC_1495.800.png" 800 534


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            ProgressiveImage.init
                { fallback = fallback
                , possiblyCached = [ cached ]
                , mainImg = mainImg
                , width = 1600
                , height = 1069
                }
    in
    ( Mo model, Cmd.map Me cmd )


view : Model -> Html Msg
view (Mo model) =
    Html.Styled.map Me <| ProgressiveImage.view model


update : Msg -> Model -> ( Model, Cmd Msg )
update (Me msg) (Mo model) =
    let
        ( newModel, newCmd ) =
            ProgressiveImage.update msg model
    in
    ( Mo newModel, Cmd.map Me newCmd )


subscriptions : Model -> Sub Msg
subscriptions (Mo model) =
    Sub.map Me <| ProgressiveImage.subscriptions model
