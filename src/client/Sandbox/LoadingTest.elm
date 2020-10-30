module Sandbox.LoadingTest exposing (..)

import Browser
import Html exposing (text)
import Url exposing (Protocol(..))
import Utils.ListUtils exposing (fromMaybe)
import Utils.Loading as Loading exposing (ManyModel, ManyMsg)


pathToUrl host basePath path =
    { protocol = Http
    , host = host
    , port_ = Nothing
    , path = "/" ++ basePath ++ path
    , query = Nothing
    , fragment = Nothing
    }


makeUrls host basePath =
    List.map
        (pathToUrl host basePath)
        [ "02_February 25-28: Around the House/DSC_4745.JPG"
        , "02_February 25-28: Around the House/DSC_4750.JPG"
        , "02_February 25-28: Around the House/DSC_4753.JPG"
        , "02_February 25-28: Around the House/DSC_4760.JPG"
        , "02_February 25-28: Around the House/DSC_4761.JPG"
        , "02_February 25-28: Around the House/DSC_4762.JPG"
        , "02_February 25-28: Around the House/DSC_4763.JPG"
        , "02_February 25-28: Around the House/DSC_4765.JPG"
        , "02_February 25-28: Around the House/DSC_4769.JPG"
        , "02_February 25-28: Around the House/DSC_4772.JPG"
        , "02_February 25-28: Around the House/DSC_4778.JPG"
        , "02_February 25-28: Around the House/DSC_4780.JPG"
        , "02_February 25-28: Around the House/DSC_4782.JPG"
        , "02_February 25-28: Around the House/DSC_4787.JPG"
        , "02_February 25-28: Around the House/DSC_4794.JPG"
        ]


type alias Model =
    { model : ManyModel Msg
    , subs : Sub Msg
    }


type Msg
    = Msg ManyMsg
    | NoOp


main : Program () Model Msg
main =
    Browser.application
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }


init url _ =
    let
        urls =
            makeUrls url.host url.path

        ( lm, lc, ls ) =
            Loading.initMany (List.take 5 urls) (List.drop 5 urls) Msg
    in
    ( { model = lm, subs = ls }, lc )


update msg model =
    ( model, Cmd.none )


view model =
    { title = "Loading Test"
    , body = [ text "hello world" ]
    }


subscriptions model =
    model.subs
