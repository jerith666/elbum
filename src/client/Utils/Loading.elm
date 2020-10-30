module Utils.Loading exposing (LoadState(..), getOneState, getState, init, initMany, update, updateMany)

import Dict exposing (Dict, fromList, get, insert, remove, values)
import Http exposing (Error, Progress, emptyBody, expectWhatever, track)
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
        { models : Dict String (LoadingModel msg)
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


initMany : List Url -> (ManyMsg -> msg) -> ( ManyModel msg, Cmd msg, Sub msg )
initMany urls wrap =
    let
        initEntry url =
            ( toString url, init (ManyMsg url >> wrap) url )

        models =
            fromList <| List.map initEntry urls
    in
    ( ManyModel
        { models = Dict.map (always <| firstOfThree >> getModel) models
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


updateMany : ManyModel msg -> ManyMsg -> ( ManyModel msg, Sub msg )
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
                ( OneModel newModel _, newSub ) =
                    update (OneModel oneModel (ManyMsg url >> mm.wrap)) loadingMsg

                otherSubs =
                    remove (toString url) mm.models
                        |> values
                        |> List.map subForOneModel
            in
            ( ManyModel
                { models = insert (toString url) newModel mm.models
                , wrap = mm.wrap
                }
            , Sub.batch <| newSub :: otherSubs
            )

        Nothing ->
            ( ManyModel mm
            , Sub.batch <| List.map subForOneModel <| values mm.models
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
