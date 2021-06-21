module Sandbox.LoadingTest exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (br, div, img, text)
import Html.Attributes exposing (height, src, width)
import Html.Events exposing (on)
import Json.Decode exposing (succeed)
import Url exposing (Protocol(..), Url, toString)
import Utils.HttpUtils exposing (viewProgress)
import Utils.Loading as Loading exposing (ManyModel, ManyMsg, getOneState, markOne)


pathToUrl host basePath path =
    { protocol = Https
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
    }


type Msg
    = Msg ManyMsg
    | Loaded Url
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


init : Url -> Key -> ( Model, Cmd Msg )
init url _ =
    let
        urls =
            makeUrls url.host url.path

        ( lm, lc ) =
            Loading.initMany (List.take 5 urls) (List.drop 5 urls) Msg
    in
    ( { urls = urls, model = lm }, lc )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Msg m ->
            let
                ( newModel, newCmd ) =
                    Loading.updateMany m model.model (always model.urls)
            in
            ( { model | model = newModel }, newCmd )

        Loaded url ->
            ( { model | model = markOne model.model url }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Loading Test"
    , body =
        List.intersperse (br [] []) <|
            List.map (viewOne model.model) model.urls
    }


viewOne model url =
    case getOneState model url of
        Just state ->
            let
                label l =
                    text <|
                        toString url
                            ++ ": "
                            ++ l

                viewOneImpl s =
                    case s of
                        Loading.NotRequested ->
                            label "not requested"

                        Loading.RequestedButNoProgress ->
                            label "requested, awaiting initial progress"

                        Loading.Loading progress ->
                            label <| viewProgress "" <| Just progress

                        Loading.Failed _ ->
                            label "failed"

                        Loading.Loaded ->
                            div []
                                [ img [ on "load" <| succeed <| Loaded url, src <| toString url, width 10, height 10 ] []
                                , label "loaded"
                                ]

                        Loading.Marked st ->
                            div [] [ text "marked: ", viewOneImpl st ]
            in
            viewOneImpl state

        Nothing ->
            text <| "mysterious missing state for " ++ toString url


subscriptions : Model -> Sub Msg
subscriptions model =
    Loading.subscriptionsMany model.model
