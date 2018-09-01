module ProgressiveImage exposing (ProgressiveImageData, ProgressiveImageModel, ProgressiveImageMsg, init, subscriptions, update, updateCmd, view, withWidthHeight)

import Album exposing (ImgSrc)
import AlbumStyles exposing (..)
import Animation exposing (px)
import Animation.Messenger exposing (..)
import Css exposing (..)
import Delay exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import ImageViews exposing (..)
import Json.Decode exposing (..)
import ResultUtils exposing (..)
import Time exposing (..)


type alias ProgressiveImageData =
    { fallback : ImgSrc
    , possiblyCached : List ImgSrc
    , mainImg : ImgSrc
    , width : Int
    , height : Int
    }


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
type ProgressiveImageStatus
    = TryingCached (List ImgSrc) ImgSrc (List ImgSrc)
    | LoadingFallback
    | LoadingMain ImgSrc
    | MainLoaded ImgSrc
    | MainOnly


type alias AnimState =
    { main : Animation.Messenger.State ProgressiveImageMsg
    , placeholder : Animation.State
    }


type ProgressiveImageModel
    = ProgImgModel ProgressiveImageData ProgressiveImageStatus AnimState


type ProgressiveImageMsg
    = Loaded ImgSrc
    | ScheduleTimeout Float Time ImgSrc
    | Timeout ImgSrc
    | MainFadeinComplete
    | AnimatePlaceholder Animation.Msg
    | AnimateMain Animation.Msg


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


init : ProgressiveImageData -> ( ProgressiveImageModel, Maybe ProgressiveImageMsg )
init data =
    let
        animState =
            { main = Animation.style hidden, placeholder = Animation.style hidden }

        model =
            case data.possiblyCached of
                [] ->
                    ProgImgModel data LoadingFallback animState

                c1 :: cOthers ->
                    ProgImgModel data (TryingCached [] c1 cOthers) animState
    in
    ( model, updateCmd model )


withWidthHeight : Int -> Int -> ProgressiveImageModel -> ProgressiveImageModel
withWidthHeight w h (ProgImgModel data status animState) =
    ProgImgModel { data | width = w, height = h } status animState


update : ProgressiveImageMsg -> ProgressiveImageModel -> ( ProgressiveImageModel, Cmd ProgressiveImageMsg )
update msg ((ProgImgModel data status animState) as oldModel) =
    case msg of
        AnimateMain animMsg ->
            let
                ( newMainState, animCmd ) =
                    Animation.Messenger.update animMsg animState.main
            in
            ( ProgImgModel data status <| { animState | main = newMainState }, animCmd )

        ScheduleTimeout n unit img ->
            ( oldModel, Delay.after n unit <| Timeout img )

        _ ->
            let
                newModel =
                    updateModel msg oldModel
            in
            ( newModel, Maybe.withDefault Cmd.none <| Maybe.map toCmd <| updateCmd newModel )


updateModel : ProgressiveImageMsg -> ProgressiveImageModel -> ProgressiveImageModel
updateModel msg ((ProgImgModel data status animState) as model) =
    case msg of
        Loaded imgSrc ->
            case status of
                TryingCached tried trying upnext ->
                    if imgSrc == trying then
                        ProgImgModel data (LoadingMain trying) { animState | placeholder = show animState.placeholder }

                    else
                        --maybe some earlier tried image?  ignore
                        model

                LoadingFallback ->
                    if imgSrc == data.fallback then
                        ProgImgModel data (LoadingMain data.fallback) { animState | placeholder = show animState.placeholder }

                    else
                        --maybe some earlier tryingCached?  ignore
                        model

                LoadingMain placeholder ->
                    if imgSrc == data.mainImg then
                        ProgImgModel data (MainLoaded placeholder) { animState | main = showMsg animState.main }

                    else
                        --something stale, ignore
                        model

                MainLoaded _ ->
                    --some stale loading notification, ignore
                    model

                MainOnly ->
                    --some stale loading notification, ignore
                    model

        Timeout imgSrc ->
            case status of
                TryingCached tried trying upnext ->
                    case upnext of
                        [] ->
                            ProgImgModel data LoadingFallback animState

                        next :: later ->
                            ProgImgModel data (TryingCached (tried ++ [ trying ]) next later) animState

                LoadingFallback ->
                    --shouldn't happen
                    model

                LoadingMain _ ->
                    -- shouldn't happen
                    model

                MainLoaded _ ->
                    --shouldn't happen
                    model

                MainOnly ->
                    --shouldn't happen
                    model

        ScheduleTimeout _ _ _ ->
            model

        MainFadeinComplete ->
            case status of
                MainLoaded _ ->
                    ProgImgModel data MainOnly { animState | placeholder = hide animState.placeholder }

                TryingCached _ _ _ ->
                    --shouldn't happen
                    model

                LoadingFallback ->
                    --shouldn't happen
                    model

                LoadingMain _ ->
                    --shouldn't happen
                    model

                MainOnly ->
                    --shouldn't happen
                    model

        AnimateMain animMsg ->
            --taken care of by caller, can't happen
            model

        AnimatePlaceholder animMsg ->
            ProgImgModel data status <| { animState | placeholder = Animation.update animMsg animState.placeholder }


updateCmd : ProgressiveImageModel -> Maybe ProgressiveImageMsg
updateCmd (ProgImgModel data status animState) =
    case status of
        TryingCached _ trying _ ->
            Just <| ScheduleTimeout 200 millisecond trying

        LoadingFallback ->
            Nothing

        LoadingMain _ ->
            Nothing

        MainLoaded oldPlaceholder ->
            Nothing

        MainOnly ->
            Nothing


subscriptions : ProgressiveImageModel -> Sub ProgressiveImageMsg
subscriptions (ProgImgModel data status animState) =
    Sub.batch
        [ Animation.subscription AnimatePlaceholder [ animState.placeholder ]
        , Animation.subscription AnimateMain [ animState.main ]
        ]


view : ProgressiveImageModel -> Html ProgressiveImageMsg
view (ProgImgModel data status animState) =
    case status of
        TryingCached tried trying nextup ->
            viewImg trying data (styledAnimation animState.placeholder) []

        LoadingFallback ->
            viewImg data.fallback data (styledAnimation animState.placeholder) []

        LoadingMain placeholder ->
            viewLoadingMain data placeholder animState.placeholder animState.main

        MainLoaded oldPlaceholder ->
            viewLoadingMain data oldPlaceholder animState.placeholder animState.main

        MainOnly ->
            viewImg data.mainImg data (styledMsgAnimation animState.main) []


viewLoadingMain : ProgressiveImageData -> ImgSrc -> Animation.State -> Animation.Messenger.State ProgressiveImageMsg -> Html ProgressiveImageMsg
viewLoadingMain data imgSrc imgSrcAnimState mainAnimState =
    div
        [ styles [ position relative ] ]
        [ viewImg data.mainImg data (styledMsgAnimation mainAnimState) [ position absolute, Css.top zero, Css.left zero, zIndex (Css.int 1) ]
        , viewImg imgSrc data (styledAnimation imgSrcAnimState) []
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
        Nothing


styledAnimation : Animation.State -> List (Attribute ProgressiveImageMsg)
styledAnimation animState =
    List.map Html.Styled.Attributes.fromUnstyled (Animation.render animState)


styledMsgAnimation : Animation.Messenger.State ProgressiveImageMsg -> List (Attribute ProgressiveImageMsg)
styledMsgAnimation animState =
    List.map Html.Styled.Attributes.fromUnstyled (Animation.render animState)
