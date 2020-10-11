module Sandbox.ImgProgressTest exposing (Model(..), Msg(..), cached, fallback, init, main, mainImg, subscriptions, update, view)

--import Css exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (..)
--import Json.Decode exposing (..)

import Album exposing (..)
import Browser exposing (..)
import Html.Styled exposing (..)
import ProgressiveImage exposing (..)
import Utils.ResultUtils exposing (toCmd)


type Model
    = Mo ProgressiveImageModel


type Msg
    = Me ProgressiveImageMsg


main : Program () Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


mainImg : ImgSrc
mainImg =
    ImgSrc "10_October 06-09: Ohiopyle Weekend/DSC_1495.JPG" 1600 1069


fallback : ImgSrc
fallback =
    ImgSrc "10_October 06-09: Ohiopyle Weekend/DSC_1495.200.png" 200 133


cached : ImgSrc
cached =
    ImgSrc "10_October 06-09: Ohiopyle Weekend/DSC_1495.800.png" 800 534


init : () -> ( Model, Cmd Msg )
init _ =
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
    ( Mo model
    , cmd
        |> Maybe.map toCmd
        |> Maybe.map (Cmd.map Me)
        |> Maybe.withDefault Cmd.none
    )


view : Model -> Document Msg
view (Mo model) =
    { title = "Image Progressiveness Test"
    , body = [ ProgressiveImage.view model |> Html.Styled.map Me |> toUnstyled ]
    }


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
