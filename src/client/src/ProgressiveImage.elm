module ProgressiveImage exposing (ProgressiveImageCompleteness(..), ProgressiveImageData, ProgressiveImageModel, ProgressiveImageMsg, cancel, init, subscriptions, update, view, withWidthHeight)

import Album exposing (ImgSrc)
import AlbumStyles exposing (..)
import Animation
import Animation.Messenger
import Css exposing (..)
import Delay exposing (..)
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes
import Html.Styled.Events exposing (..)
import Http exposing (Progress)
import ImageViews exposing (..)
import Json.Decode exposing (..)
import Url exposing (Url)
import Utils.HttpUtils exposing (appendPath)
import Utils.ListUtils exposing (encodePath)
import Utils.Loading as Loading exposing (LoadState, LoadingMsg, OneModel, cmdFor, getState)
import Utils.ResultUtils exposing (..)


{-| control flow:

1.  try some images that might already be cached.
    leave these images hidden while we load them, but timeout quickly if they don't show up.
    if one does show up, skip the fallback.

2.  if none of the cached images loads, load a fallback image.
    TODO maybe let it be visible from the get-go, as on really slow connections even this may take some time?

3.  once we have some placeholder image loaded, fade it in with a CSS animation -- this requires carefully choreographed DOM updates.
    meanwhile, begin loading the main image. keep it hidden while it loads.

4.  once the main image is loaded, cross-fade from the placeholder to the main image.
    again, this requires careful choreography.

-}
type alias ProgressiveImageData =
    { fallback : ImgSrc
    , possiblyCached : List ImgSrc
    , mainImg : ImgSrc
    , width : Int
    , height : Int
    , baseUrl : Url
    }


type ProgressiveImageStatus
    = TryingCached (List ImgSrc) ImgSrc (List ImgSrc)
    | LoadingFallback
    | LoadingMain ImgSrc (OneModel ProgressiveImageMsg)
    | MainLoaded ImgSrc
    | MainOnly


type ProgressiveImageCompleteness
    = Incomplete
    | Complete


type alias AnimState =
    { main : Animation.Messenger.State ProgressiveImageMsg
    , placeholder : Animation.State
    }


type ProgressiveImageModel
    = ProgImgModel ProgImgModelRec


type alias ProgImgModelRec =
    { data : ProgressiveImageData
    , status : ProgressiveImageStatus
    , animState : AnimState
    , completeness : ProgressiveImageCompleteness
    }


type ProgressiveImageMsg
    = Loaded ImgSrc
    | ScheduleTimeout Int ImgSrc
    | Timeout ImgSrc
    | MainFadeinComplete
    | AnimatePlaceholder Animation.Msg
    | AnimateMain Animation.Msg
    | NestedLoadingMsg LoadingMsg


hidden : List Animation.Property
hidden =
    [ Animation.opacity 0 ]


hide : Animation.State -> Animation.State
hide =
    Animation.interrupt [ Animation.to hidden ]


shown : List Animation.Property
shown =
    [ Animation.opacity 1 ]


show : Animation.State -> Animation.State
show =
    Animation.interrupt [ Animation.to shown ]


showMsg : Animation.Messenger.State ProgressiveImageMsg -> Animation.Messenger.State ProgressiveImageMsg
showMsg =
    Animation.interrupt [ Animation.to shown, Animation.Messenger.send MainFadeinComplete ]


init : ProgressiveImageData -> ( ProgressiveImageModel, Cmd ProgressiveImageMsg )
init data =
    let
        animState =
            { main = Animation.style hidden, placeholder = Animation.style hidden }

        model =
            case data.possiblyCached of
                [] ->
                    ProgImgModel { data = data, status = LoadingFallback, animState = animState, completeness = Incomplete }

                c1 :: cOthers ->
                    ProgImgModel { data = data, status = TryingCached [] c1 cOthers, animState = animState, completeness = Incomplete }
    in
    ( model, updateCmd model )


withWidthHeight : Int -> Int -> ProgressiveImageModel -> ProgressiveImageModel
withWidthHeight w h (ProgImgModel piModel) =
    let
        dataOldWH =
            piModel.data
    in
    ProgImgModel { piModel | data = { dataOldWH | width = w, height = h } }


update : ProgressiveImageMsg -> ProgressiveImageModel -> ( ProgressiveImageModel, Cmd ProgressiveImageMsg, ProgressiveImageCompleteness )
update msg oldModel =
    let
        ( ProgImgModel piModel, cmd ) =
            updateImpl msg oldModel
    in
    ( ProgImgModel piModel, cmd, piModel.completeness )


updateImpl : ProgressiveImageMsg -> ProgressiveImageModel -> ( ProgressiveImageModel, Cmd ProgressiveImageMsg )
updateImpl msg ((ProgImgModel piModel) as oldModel) =
    case msg of
        AnimateMain animMsg ->
            let
                oldAnimState =
                    piModel.animState

                ( newMainState, animCmd ) =
                    Animation.Messenger.update animMsg piModel.animState.main
            in
            ( ProgImgModel { piModel | animState = { oldAnimState | main = newMainState } }, animCmd )

        ScheduleTimeout n img ->
            ( oldModel, Delay.after n <| Timeout img )

        _ ->
            let
                newModel =
                    updateModel msg oldModel
            in
            ( newModel, updateCmd newModel )


updateModel : ProgressiveImageMsg -> ProgressiveImageModel -> ProgressiveImageModel
updateModel msg ((ProgImgModel piModel) as model) =
    case msg of
        Loaded imgSrc ->
            case piModel.status of
                TryingCached _ trying _ ->
                    if imgSrc == trying then
                        let
                            oldAnimState =
                                piModel.animState

                            ( loadingModel, _ ) =
                                Loading.init NestedLoadingMsg <| appendPath piModel.data.baseUrl <| encodePath piModel.data.mainImg.url
                        in
                        ProgImgModel
                            { piModel
                                | status = LoadingMain trying loadingModel
                                , animState = { oldAnimState | placeholder = show piModel.animState.placeholder }
                            }

                    else
                        --maybe some earlier tried image?  ignore
                        model

                LoadingFallback ->
                    if imgSrc == piModel.data.fallback then
                        let
                            oldAnimState =
                                piModel.animState

                            ( loadingModel, _ ) =
                                Loading.init NestedLoadingMsg <| appendPath piModel.data.baseUrl <| encodePath piModel.data.mainImg.url
                        in
                        ProgImgModel
                            { piModel
                                | status = LoadingMain piModel.data.fallback loadingModel
                                , animState = { oldAnimState | placeholder = show piModel.animState.placeholder }
                            }

                    else
                        --maybe some earlier tryingCached?  ignore
                        model

                LoadingMain placeholder _ ->
                    if imgSrc == piModel.data.mainImg then
                        mainLoaded piModel placeholder

                    else
                        --something stale, ignore
                        model

                MainLoaded _ ->
                    --some stale loading notification, ignore
                    model

                MainOnly ->
                    --some stale loading notification, ignore
                    model

        Timeout timedOut ->
            case piModel.status of
                TryingCached tried trying upnext ->
                    case timedOut == trying of
                        True ->
                            case upnext of
                                [] ->
                                    ProgImgModel { piModel | status = LoadingFallback }

                                next :: later ->
                                    ProgImgModel { piModel | status = TryingCached (tried ++ [ trying ]) next later }

                        False ->
                            --stale timeout
                            model

                LoadingFallback ->
                    --shouldn't happen
                    model

                LoadingMain _ _ ->
                    -- shouldn't happen
                    model

                MainLoaded _ ->
                    --shouldn't happen
                    model

                MainOnly ->
                    --shouldn't happen
                    model

        ScheduleTimeout _ _ ->
            model

        MainFadeinComplete ->
            case piModel.status of
                MainLoaded _ ->
                    let
                        oldAnimState =
                            piModel.animState
                    in
                    ProgImgModel
                        { piModel
                            | status = MainOnly
                            , animState = { oldAnimState | placeholder = hide piModel.animState.placeholder }
                            , completeness = Complete
                        }

                TryingCached _ _ _ ->
                    --shouldn't happen
                    model

                LoadingFallback ->
                    --shouldn't happen
                    model

                LoadingMain _ _ ->
                    --shouldn't happen
                    model

                MainOnly ->
                    --shouldn't happen
                    model

        AnimateMain _ ->
            --taken care of by caller, can't happen
            model

        AnimatePlaceholder animMsg ->
            let
                oldAnimState =
                    piModel.animState
            in
            ProgImgModel { piModel | animState = { oldAnimState | placeholder = Animation.update animMsg piModel.animState.placeholder } }

        NestedLoadingMsg nestedMsg ->
            case piModel.status of
                LoadingMain placeholder loadingState ->
                    let
                        newLoadingState =
                            Loading.update nestedMsg loadingState
                    in
                    case getState newLoadingState of
                        Loading.Loaded ->
                            mainLoaded piModel placeholder

                        _ ->
                            ProgImgModel { piModel | status = LoadingMain placeholder newLoadingState }

                TryingCached _ _ _ ->
                    model

                LoadingFallback ->
                    model

                MainLoaded _ ->
                    model

                MainOnly ->
                    model


mainLoaded : ProgImgModelRec -> ImgSrc -> ProgressiveImageModel
mainLoaded piModel placeholder =
    let
        oldAnimState =
            piModel.animState
    in
    ProgImgModel
        { piModel
            | status = MainLoaded placeholder
            , animState = { oldAnimState | main = showMsg piModel.animState.main }
            , completeness = Complete
        }


updateCmd : ProgressiveImageModel -> Cmd ProgressiveImageMsg
updateCmd (ProgImgModel piModel) =
    case piModel.status of
        TryingCached _ trying _ ->
            toCmd <| ScheduleTimeout 200 trying

        LoadingFallback ->
            Cmd.none

        LoadingMain _ loadingState ->
            cmdFor loadingState

        MainLoaded _ ->
            Cmd.none

        MainOnly ->
            Cmd.none


subscriptions : ProgressiveImageModel -> Sub ProgressiveImageMsg
subscriptions (ProgImgModel piModel) =
    let
        animSubs =
            Sub.batch
                [ Animation.subscription AnimatePlaceholder [ piModel.animState.placeholder ]
                , Animation.subscription AnimateMain [ piModel.animState.main ]
                ]

        loadingSubs =
            case piModel.status of
                LoadingMain _ loadingState ->
                    Loading.subscriptions loadingState

                TryingCached _ _ _ ->
                    Sub.none

                LoadingFallback ->
                    Sub.none

                MainLoaded _ ->
                    Sub.none

                MainOnly ->
                    Sub.none
    in
    Sub.batch [ animSubs, loadingSubs ]


cancel : ProgressiveImageModel -> Cmd msg
cancel (ProgImgModel m) =
    case m.status of
        TryingCached _ _ _ ->
            Cmd.none

        LoadingFallback ->
            Cmd.none

        LoadingMain _ oneModel ->
            Loading.cancel oneModel

        MainLoaded _ ->
            Cmd.none

        MainOnly ->
            Cmd.none


view : ProgressiveImageModel -> ( Html ProgressiveImageMsg, Maybe Progress )
view (ProgImgModel piModel) =
    case piModel.status of
        TryingCached _ trying _ ->
            ( viewImg trying piModel.data (styledAnimation piModel.animState.placeholder) [], Nothing )

        LoadingFallback ->
            ( viewImg piModel.data.fallback piModel.data (styledAnimation piModel.animState.placeholder) [], Nothing )

        LoadingMain placeholder loadingState ->
            ( viewImg placeholder piModel.data (styledAnimation piModel.animState.placeholder) [], getProgress loadingState )

        MainLoaded oldPlaceholder ->
            ( viewMainLoaded piModel.data oldPlaceholder piModel.animState.placeholder piModel.animState.main, Nothing )

        MainOnly ->
            ( viewImg piModel.data.mainImg piModel.data (styledMsgAnimation piModel.animState.main) [], Nothing )


getProgress : OneModel msg -> Maybe Progress
getProgress =
    getProgressImpl << getState


getProgressImpl : LoadState -> Maybe Progress
getProgressImpl loadState =
    case loadState of
        Loading.NotRequested ->
            Nothing

        Loading.RequestedButNoProgress ->
            Nothing

        Loading.Loading progress ->
            Just progress

        Loading.Loaded ->
            Nothing

        Loading.Marked ls ->
            getProgressImpl ls

        Loading.Failed _ ->
            Nothing


viewMainLoaded : ProgressiveImageData -> ImgSrc -> Animation.State -> Animation.Messenger.State ProgressiveImageMsg -> Html ProgressiveImageMsg
viewMainLoaded data imgSrc imgSrcAnimState mainAnimState =
    div
        [ styles [ position relative ] ]
        [ viewImg
            data.mainImg
            data
            (styledMsgAnimation mainAnimState)
            [ position absolute, Css.top zero, Css.left zero, zIndex (Css.int 1) ]
        , viewImg
            imgSrc
            data
            (styledAnimation imgSrcAnimState)
            []
        ]


viewImg : ImgSrc -> ProgressiveImageData -> List (Attribute ProgressiveImageMsg) -> List Style -> Html ProgressiveImageMsg
viewImg imgSrc data animStyle styles =
    renderPresized
        0
        data.width
        data.height
        imgSrc
        []
        styles
        (animStyle ++ [ on "load" <| succeed <| Loaded imgSrc ])


styledAnimation : Animation.State -> List (Attribute ProgressiveImageMsg)
styledAnimation animState =
    List.map Html.Styled.Attributes.fromUnstyled (Animation.render animState)


styledMsgAnimation : Animation.Messenger.State ProgressiveImageMsg -> List (Attribute ProgressiveImageMsg)
styledMsgAnimation animState =
    List.map Html.Styled.Attributes.fromUnstyled (Animation.render animState)
