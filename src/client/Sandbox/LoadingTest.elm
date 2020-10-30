module Sandbox.LoadingTest exposing (..)

import Browser
import Html exposing (br, text)
import Url exposing (Protocol(..), Url, toString)
import Utils.HttpUtils exposing (viewProgress)
import Utils.ListUtils exposing (fromMaybe)
import Utils.Loading as Loading exposing (ManyModel, ManyMsg, getOneState, getState)


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
    { urls : List Url
    , model : ManyModel Msg
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
    ( { urls = urls, model = lm, subs = ls }, lc )


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Msg m ->
            let
                ( newModel, newCmd, newSub ) =
                    Loading.updateMany m model.model
            in
            ( { urls = model.urls, model = newModel, subs = newSub }, newCmd )


view model =
    { title = "Loading Test"
    , body =
        List.intersperse (br [] []) <|
            List.map (viewOne model.model) model.urls
    }


viewOne model url =
    text <|
        case getOneState model url of
            Just state ->
                toString url
                    ++ ": "
                    ++ (case state of
                            Loading.NotStarted ->
                                "not started"

                            Loading.Loading progress ->
                                viewProgress "" <| Just progress

                            Loading.Loaded ->
                                "loaded"

                            Loading.Failed error ->
                                "failed"
                       )

            Nothing ->
                "mysterious missing state for " ++ toString url


subscriptions model =
    model.subs
