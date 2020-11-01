module Utils.Loading exposing (LoadState(..), LoadingMsg, ManyModel, ManyMsg, cmdFor, cmdForMany, getOneState, getState, init, initMany, subscriptions, subscriptionsMany, update, updateMany)

import Dict exposing (Dict, filter, fromList, get, insert, size, toList, values)
import Http exposing (Error, Progress, emptyBody, expectWhatever, track)
import List exposing (head, length, tail)
import Task
import Url exposing (Url, fromString, toString)


type LoadState
    = NotRequested
    | RequestedButNoProgress
    | Loading Progress
    | Loaded
    | Failed Error


type LoadingMsg
    = Requested
    | GotProgress Progress
    | Finished
    | Failure Error


type LoadingModel msg
    = LoadingModel
        { url : Url
        , state : LoadState
        , tracker : String
        }


type OneModel msg
    = OneModel (LoadingModel msg) (LoadingMsg -> msg)


type ManyModel msg
    = ManyModel
        { pending : List Url
        , maxConcurrentCount : Int
        , models : Dict String (LoadingModel msg)
        , wrap : ManyMsg -> msg
        }


type ManyMsg
    = ManyMsg Url LoadingMsg


init : (LoadingMsg -> msg) -> Url -> ( OneModel msg, Cmd msg )
init wrap url =
    let
        tracker =
            "LoadingTracker:" ++ toString url

        model =
            OneModel
                (LoadingModel
                    { url = url
                    , state = NotRequested
                    , tracker = tracker
                    }
                )
                wrap
    in
    ( model
    , cmdFor model
    )


initMany : List Url -> List Url -> (ManyMsg -> msg) -> ( ManyModel msg, Cmd msg )
initMany firstUrls restUrls wrap =
    let
        initEntry url =
            ( toString url, init (ManyMsg url >> wrap) url )

        models =
            fromList <| List.map initEntry firstUrls
    in
    ( ManyModel
        { pending = restUrls
        , maxConcurrentCount = length firstUrls
        , models = Dict.map (always <| Tuple.first >> getModel) models
        , wrap = wrap
        }
    , Cmd.batch <| values <| Dict.map (always Tuple.second) models
    )


update : LoadingMsg -> OneModel msg -> OneModel msg
update msg (OneModel (LoadingModel m) wrap) =
    case msg of
        Requested ->
            case m.state of
                NotRequested ->
                    OneModel (LoadingModel { m | state = RequestedButNoProgress }) wrap

                _ ->
                    OneModel (LoadingModel m) wrap

        GotProgress progress ->
            let
                updatedWithProgress =
                    OneModel (LoadingModel { m | state = Loading progress }) wrap

                ignoreStaleProgress =
                    OneModel (LoadingModel m) wrap
            in
            case m.state of
                NotRequested ->
                    updatedWithProgress

                RequestedButNoProgress ->
                    updatedWithProgress

                Loading _ ->
                    updatedWithProgress

                Loaded ->
                    ignoreStaleProgress

                Failed _ ->
                    ignoreStaleProgress

        Finished ->
            OneModel (LoadingModel { m | state = Loaded }) wrap

        Failure error ->
            OneModel (LoadingModel { m | state = Failed error }) wrap


updateMany : ManyMsg -> ManyModel msg -> (List Url -> List Url) -> ( ManyModel msg, Cmd msg )
updateMany (ManyMsg url loadingMsg) (ManyModel mm) revisePending =
    case get (toString url) mm.models of
        Just oneModel ->
            let
                (OneModel (LoadingModel oneNewModel) _) =
                    update loadingMsg (OneModel oneModel (ManyMsg url >> mm.wrap))

                oneNewModels =
                    insert (toString url) (LoadingModel oneNewModel) mm.models

                revisedPending =
                    case isLoading <| LoadingModel oneNewModel of
                        True ->
                            revisePending mm.pending

                        False ->
                            List.filter ((/=) url) <| revisePending <| List.filter ((/=) url) mm.pending

                ( allNewModels, newPending, newCmd ) =
                    promotePending mm.wrap mm.maxConcurrentCount oneNewModels revisedPending
            in
            ( ManyModel
                { pending = newPending
                , maxConcurrentCount = mm.maxConcurrentCount
                , models = allNewModels
                , wrap = mm.wrap
                }
            , newCmd
            )

        Nothing ->
            ( ManyModel mm
            , Cmd.none
            )


isLoading (LoadingModel m) =
    case m.state of
        NotRequested ->
            True

        RequestedButNoProgress ->
            True

        Loading _ ->
            True

        Loaded ->
            False

        Failed _ ->
            False


promotePending : (ManyMsg -> msg) -> Int -> Dict String (LoadingModel msg) -> List Url -> ( Dict String (LoadingModel msg), List Url, Cmd msg )
promotePending wrap maxConcurrentCount currentModels pending =
    let
        inProgCount =
            size <| filter (always isLoading) currentModels
    in
    case inProgCount >= maxConcurrentCount of
        True ->
            ( currentModels, pending, Cmd.none )

        False ->
            case head pending of
                Nothing ->
                    ( currentModels, pending, Cmd.none )

                Just nextUrl ->
                    let
                        ( OneModel oneNewModel _, oneNewCmd ) =
                            init (wrap << ManyMsg nextUrl) nextUrl
                    in
                    ( insert (toString nextUrl) oneNewModel currentModels
                    , Maybe.withDefault [] <| tail pending
                    , oneNewCmd
                    )


subscriptions : OneModel msg -> Sub msg
subscriptions (OneModel (LoadingModel lm) wrap) =
    let
        trackIt =
            track lm.tracker <| wrap << GotProgress

        ignoreIt =
            Sub.none
    in
    case lm.state of
        NotRequested ->
            ignoreIt

        RequestedButNoProgress ->
            trackIt

        Loading _ ->
            trackIt

        Loaded ->
            ignoreIt

        Failed _ ->
            ignoreIt


subscriptionsMany : ManyModel msg -> Sub msg
subscriptionsMany (ManyModel mm) =
    let
        subForOneModel ( oneUrlStr, LoadingModel m ) =
            case fromString oneUrlStr of
                Nothing ->
                    []

                Just oneUrl ->
                    [ subscriptions <| OneModel (LoadingModel m) <| ManyMsg oneUrl >> mm.wrap ]
    in
    mm.models |> toList |> List.concatMap subForOneModel |> Sub.batch


cmdFor : OneModel msg -> Cmd msg
cmdFor (OneModel (LoadingModel m) wrap) =
    let
        handle : Result Error () -> LoadingMsg
        handle result =
            case result of
                Ok _ ->
                    Finished

                Err err ->
                    Failure err

        get =
            Http.request
                { method = "GET"
                , headers = []
                , url = toString m.url
                , body = emptyBody
                , expect = expectWhatever handle
                , timeout = Nothing
                , tracker = Just m.tracker
                }

        requested =
            Task.perform identity <| Task.succeed Requested
    in
    Cmd.map wrap <|
        case m.state of
            NotRequested ->
                Cmd.batch [ get, requested ]

            RequestedButNoProgress ->
                Cmd.none

            Loading _ ->
                Cmd.none

            Loaded ->
                Cmd.none

            Failed _ ->
                Cmd.none


cmdForMany : ManyModel msg -> Cmd msg
cmdForMany (ManyModel mm) =
    let
        cmdForOneModel ( oneUrlStr, LoadingModel m ) =
            case fromString oneUrlStr of
                Nothing ->
                    []

                Just oneUrl ->
                    [ cmdFor <| OneModel (LoadingModel m) <| mm.wrap << ManyMsg oneUrl ]
    in
    Cmd.batch <| List.concatMap cmdForOneModel <| Dict.toList mm.models


getState : OneModel msg -> LoadState
getState (OneModel (LoadingModel m) _) =
    m.state


getOneState : ManyModel msg -> Url -> Maybe LoadState
getOneState (ManyModel mm) url =
    case get (toString url) mm.models of
        Just (LoadingModel m) ->
            Just m.state

        Nothing ->
            case List.member url mm.pending of
                True ->
                    Just NotRequested

                False ->
                    Nothing


getModel : OneModel msg -> LoadingModel msg
getModel (OneModel m _) =
    m
