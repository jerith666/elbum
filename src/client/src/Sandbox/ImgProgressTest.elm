module Sandbox.ImgProgressTest exposing (Model(..), Msg(..), main)

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
    = Mo ProgressiveImageModel ProgressiveImageCompleteness


type Msg
    = Me ProgressiveImageMsg


main : Program () Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias ImgInfo =
    { name : String, x : Int, y : Int }


baseImg : ImgInfo
baseImg =
    { name = "02_February 25-28: Around the House/DSC_4742"
    , x = 2992
    , y = 2000
    }


mainImg : ImgSrc
mainImg =
    ImgSrc (baseImg.name ++ ".JPG") baseImg.x baseImg.y


fallback : ImgSrc
fallback =
    img 200


cached : ImgSrc
cached =
    img 800


img : Int -> ImgSrc
img width =
    case width == baseImg.x of
        True ->
            ImgSrc (baseImg.name ++ ".JPG") baseImg.x baseImg.y

        False ->
            let
                ratio =
                    toFloat width / toFloat baseImg.x

                y =
                    floor <| ratio * toFloat baseImg.y
            in
            ImgSrc (baseImg.name ++ "." ++ String.fromInt width ++ ".png") width y


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
    ( Mo model Incomplete
    , cmd
        |> Maybe.map toCmd
        |> Maybe.map (Cmd.map Me)
        |> Maybe.withDefault Cmd.none
    )


view : Model -> Document Msg
view (Mo model completeness) =
    let
        prefix =
            case completeness of
                Incomplete ->
                    "[incomplete] "

                Complete ->
                    "[complete] "
    in
    { title = prefix ++ "Image Progressiveness Test"
    , body = [ ProgressiveImage.view model |> Html.Styled.map Me |> toUnstyled ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update (Me msg) (Mo model _) =
    let
        ( newModel, newCmd, completeness ) =
            ProgressiveImage.update msg model
    in
    ( Mo newModel completeness, Cmd.map Me newCmd )


subscriptions : Model -> Sub Msg
subscriptions (Mo model _) =
    Sub.map Me <| ProgressiveImage.subscriptions model
