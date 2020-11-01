module Utils.Loading exposing (LoadState(..), LoadingMsg, ManyModel, ManyMsg, cmdFor, cmdForMany, getOneState, getState, init, initMany, subscriptions, subscriptionsMany, update, updateMany)

import Dict exposing (Dict, filter, fromList, get, insert, size, toList, values)
import Http exposing (Error, Progress, emptyBody, expectWhatever, track)
import List exposing (head, length, tail)
import Url exposing (Url, fromString, toString)


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
                    , state = NotStarted
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


update : LoadingMsg -> OneModel msg -> ( OneModel msg, Sub msg )
update msg (OneModel (LoadingModel m) wrap) =
    case msg of
        GotProgress progress ->
            let
                updatedWithProgress =
                    ( OneModel (LoadingModel { m | state = Loading progress }) wrap
                    , track m.tracker <| wrap << GotProgress
                    )

                ignoreStaleProgress =
                    ( OneModel (LoadingModel m) wrap
                    , Sub.none
                    )
            in
            case m.state of
                NotStarted ->
                    updatedWithProgress

                Loading _ ->
                    updatedWithProgress

                Loaded ->
                    ignoreStaleProgress

                Failed _ ->
                    ignoreStaleProgress

        Finished ->
            ( OneModel (LoadingModel { m | state = Loaded }) wrap
            , Sub.none
            )

        Failure error ->
            ( OneModel (LoadingModel { m | state = Failed error }) wrap
            , Sub.none
            )


updateMany : ManyMsg -> ManyModel msg -> (List Url -> List Url) -> ( ManyModel msg, Cmd msg )
updateMany (ManyMsg url loadingMsg) (ManyModel mm) revisePending =
    case get (toString url) mm.models of
        Just oneModel ->
            let
                ( OneModel (LoadingModel oneNewModel) _, _ ) =
                    update loadingMsg (OneModel oneModel (ManyMsg url >> mm.wrap))

                oneNewModels =
                    insert (toString url) (LoadingModel oneNewModel) mm.models

                ( allNewModels, newPending, newCmd ) =
                    promotePending mm.wrap mm.maxConcurrentCount oneNewModels <| revisePending mm.pending
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


promotePending : (ManyMsg -> msg) -> Int -> Dict String (LoadingModel msg) -> List Url -> ( Dict String (LoadingModel msg), List Url, Cmd msg )
promotePending wrap maxConcurrentCount currentModels pending =
    let
        isLoading (LoadingModel m) =
            case m.state of
                Loading _ ->
                    True

                NotStarted ->
                    True

                _ ->
                    False

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
    track lm.tracker <| wrap << GotProgress


subscriptionsMany : ManyModel msg -> Sub msg
subscriptionsMany (ManyModel mm) =
    let
        subForOneModel ( oneUrlStr, LoadingModel m ) =
            case fromString oneUrlStr of
                Nothing ->
                    []

                Just oneUrl ->
                    case m.state of
                        NotStarted ->
                            [ track m.tracker (GotProgress >> ManyMsg oneUrl >> mm.wrap) ]

                        Loading _ ->
                            [ track m.tracker (GotProgress >> ManyMsg oneUrl >> mm.wrap) ]

                        Loaded ->
                            []

                        Failed _ ->
                            []
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
    in
    Cmd.map wrap <|
        case m.state of
            NotStarted ->
                get

            Loading _ ->
                get

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
                    Just NotStarted

                False ->
                    Nothing


getModel : OneModel msg -> LoadingModel msg
getModel (OneModel m _) =
    m
