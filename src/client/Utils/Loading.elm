module Utils.Loading exposing (LoadState)

import Http exposing (Error, Progress, emptyBody, expectWhatever)
import Url exposing (Url, toString)


type LoadState
    = NotStarted
    | Loading Progress
    | Loaded
    | Failed Error


type LoadingMsg
    = GotProgress Progress
    | Finished
    | Failure Error


type LoadingModel msg
    = LoadingModel
        { url : Url
        , state : LoadState
        , wrap : LoadingMsg -> msg
        , tracker : String
        }


getState : LoadingModel msg -> LoadState
getState (LoadingModel m) =
    m.state


init : Url -> (LoadingMsg -> msg) -> ( LoadingModel msg, Cmd msg, Sub msg )
init url wrap =
    let
        tracker =
            "LoadingTracker:" ++ toString url

        handle : Result Error () -> LoadingMsg
        handle result =
            case result of
                Ok _ ->
                    Finished

                Err err ->
                    Failure err
    in
    ( LoadingModel
        { url = url
        , state = NotStarted
        , wrap = wrap
        , tracker = tracker
        }
    , Cmd.map wrap <|
        Http.request
            { method = "GET"
            , headers = []
            , url = toString url
            , body = emptyBody
            , expect = expectWhatever handle
            , timeout = Nothing
            , tracker = Just tracker
            }
    , Http.track tracker <| wrap << GotProgress
    )


update : LoadingModel msg -> LoadingMsg -> ( LoadingModel msg, Sub msg )
update (LoadingModel m) msg =
    case msg of
        GotProgress progress ->
            ( LoadingModel { m | state = Loading progress }
            , Http.track m.tracker <| m.wrap << GotProgress
            )

        Finished ->
            ( LoadingModel { m | state = Loaded }
            , Sub.none
            )

        Failure error ->
            ( LoadingModel { m | state = Failed error }
            , Sub.none
            )
