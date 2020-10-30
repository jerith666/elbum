module Utils.Loading exposing (LoadState(..), LoadingMsg, ManyModel, ManyMsg, getOneState, getState, init, initMany, update, updateMany)

import Dict exposing (Dict, filter, fromList, get, insert, size, values)
import Http exposing (Error, Progress, emptyBody, expectWhatever, track)
import List exposing (head, length, tail)
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


init : (LoadingMsg -> msg) -> Url -> ( OneModel msg, Cmd msg, Sub msg )
init wrap url =
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
    ( OneModel
        (LoadingModel
            { url = url
            , state = NotStarted
            , tracker = tracker
            }
        )
        wrap
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
    , track tracker <| wrap << GotProgress
    )


initMany : List Url -> List Url -> (ManyMsg -> msg) -> ( ManyModel msg, Cmd msg, Sub msg )
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
        , models = Dict.map (always <| firstOfThree >> getModel) models
        , wrap = wrap
        }
    , Cmd.batch <| values <| Dict.map (always secondOfThree) models
    , Sub.batch <| values <| Dict.map (always thirdOfThree) models
    )


update : OneModel msg -> LoadingMsg -> ( OneModel msg, Sub msg )
update (OneModel (LoadingModel m) wrap) msg =
    case msg of
        GotProgress progress ->
            ( OneModel (LoadingModel { m | state = Loading progress }) wrap
            , track m.tracker <| wrap << GotProgress
            )

        Finished ->
            ( OneModel (LoadingModel { m | state = Loaded }) wrap
            , Sub.none
            )

        Failure error ->
            ( OneModel (LoadingModel { m | state = Failed error }) wrap
            , Sub.none
            )


updateMany : ManyModel msg -> ManyMsg -> ( ManyModel msg, Cmd msg, Sub msg )
updateMany (ManyModel mm) (ManyMsg url loadingMsg) =
    let
        subForOneModel (LoadingModel m) =
            case m.state of
                NotStarted ->
                    Sub.none

                Loading _ ->
                    track m.tracker (GotProgress >> ManyMsg url >> mm.wrap)

                Loaded ->
                    Sub.none

                Failed _ ->
                    Sub.none
    in
    case get (toString url) mm.models of
        Just oneModel ->
            let
                ( OneModel (LoadingModel oneNewModel) _, _ ) =
                    update (OneModel oneModel (ManyMsg url >> mm.wrap)) loadingMsg

                oneNewModels =
                    insert (toString url) (LoadingModel oneNewModel) mm.models

                ( allNewModels, newPending, newCmd ) =
                    promotePending mm.wrap mm.maxConcurrentCount oneNewModels mm.pending

                subs =
                    allNewModels
                        |> values
                        |> List.map subForOneModel
            in
            ( ManyModel
                { pending = newPending
                , maxConcurrentCount = mm.maxConcurrentCount
                , models = allNewModels
                , wrap = mm.wrap
                }
            , newCmd
            , Sub.batch subs
            )

        Nothing ->
            ( ManyModel mm
            , Cmd.none
            , Sub.batch <| List.map subForOneModel <| values mm.models
            )


promotePending : (ManyMsg -> msg) -> Int -> Dict String (LoadingModel msg) -> List Url -> ( Dict String (LoadingModel msg), List Url, Cmd msg )
promotePending wrap maxConcurrentCount currentModels pending =
    let
        isLoading (LoadingModel m) =
            case m.state of
                Loading _ ->
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
                        ( OneModel oneNewModel _, oneNewCmd, _ ) =
                            init (wrap << ManyMsg nextUrl) nextUrl
                    in
                    ( insert (toString nextUrl) oneNewModel currentModels
                    , Maybe.withDefault [] <| tail pending
                    , oneNewCmd
                    )


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


firstOfThree ( a, _, _ ) =
    a


secondOfThree ( _, b, _ ) =
    b


thirdOfThree ( _, _, c ) =
    c
