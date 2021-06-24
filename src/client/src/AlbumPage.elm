module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), ViewportInfo, baseAlbumOf, cmdFor, eqIgnoringVpInfo, getImgPosition, hashForAlbum, initThumbs, initThumbsFullVp, pageSize, progInit, subscriptions, titleOf, update, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import FullImagePage exposing (..)
import Html.Events.Extra.Touch exposing (..)
import Html.Styled exposing (..)
import ImageViews exposing (..)
import Json.Decode exposing (..)
import List exposing (take)
import ProgressiveImage exposing (..)
import Task
import ThumbPage exposing (..)
import Url exposing (Url)
import Utils.AlbumUtils exposing (..)
import Utils.KeyboardUtils exposing (onEscape)
import Utils.ListUtils exposing (..)
import Utils.Loading exposing (ManyModel, ManyMsg, cmdForMany, initMany, markOne, subscriptionsMany, updateMany, updatePending)
import Utils.LocationUtils exposing (AnchorFunction)
import Utils.ResultUtils exposing (..)
import Utils.TouchUtils as TU exposing (..)


type AlbumPage
    = Thumbs
        { album : Album
        , vpInfo : ViewportInfo
        , baseUrl : Url
        , imageLoader : ManyModel AlbumPageMsg
        }
    | FullImage
        { prevImgs : List Image
        , album : Album
        , progModel : ProgressiveImageModel
        , vpInfo : ViewportInfo
        , scroll : Maybe Float
        , touchState : TouchState
        , imgPosition : Maybe Element
        , baseUrl : Url
        , imageLoader : ManyModel AlbumPageMsg
        }


type alias ViewportInfo =
    { bodyViewport : Viewport, rootDivViewport : Maybe Viewport }


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | TouchDragStart Event
    | TouchDragContinue Event
    | TouchDragAbandon
    | TouchEndZoom TouchState
    | Prev
    | Next
    | BackToThumbs
    | FullMsg ProgressiveImageMsg
    | ImgPositionFailed
    | GotImgPosition Element
    | LoadingMsg ManyMsg
    | ThumbLoaded Url
    | NoUpdate


initThumbs : Album -> Viewport -> Url -> ( AlbumPage, Cmd AlbumPageMsg )
initThumbs album bodyViewport baseUrl =
    initThumbsFullVp album
        { bodyViewport = bodyViewport
        , rootDivViewport = Nothing
        }
        baseUrl


initThumbsFullVp : Album -> ViewportInfo -> Url -> ( AlbumPage, Cmd AlbumPageMsg )
initThumbsFullVp album vpInfo baseUrl =
    let
        ( emptyLoader, _ ) =
            initMany [] [] <| always ()

        baseModel =
            { album = album
            , vpInfo = vpInfo
            , baseUrl = baseUrl
            , imageLoader = emptyLoader
            }

        firstUrls =
            take 5 <| ThumbPage.urlsToGet <| thumbModel baseModel

        restUrls =
            List.filter (\u -> not <| List.member u firstUrls) <| allUrls baseUrl <| thumbModel baseModel

        ( imageLoader, imgCmd ) =
            initMany firstUrls restUrls LoadingMsg
    in
    ( Thumbs
        { album = album
        , vpInfo = vpInfo
        , baseUrl = baseUrl
        , imageLoader = imageLoader
        }
    , imgCmd
    )


update : AlbumPageMsg -> AlbumPage -> Maybe Float -> ( AlbumPage, Cmd AlbumPageMsg )
update msg model scroll =
    case msg of
        View prevImgs curImg nextImgs ->
            case model of
                Thumbs th ->
                    let
                        ( w, h ) =
                            fitImage
                                curImg.srcSetFirst
                                (floor th.vpInfo.bodyViewport.viewport.width)
                                (floor th.vpInfo.bodyViewport.viewport.height)

                        ( progModel, progCmd ) =
                            progInit th.vpInfo.bodyViewport curImg w h
                    in
                    ( FullImage
                        { prevImgs = prevImgs
                        , album =
                            { title = th.album.title
                            , imageFirst = curImg
                            , imageRest = nextImgs
                            , thumbnail = th.album.thumbnail
                            }
                        , progModel = progModel
                        , vpInfo = th.vpInfo
                        , scroll = scroll
                        , touchState = TU.init
                        , imgPosition = Nothing
                        , baseUrl = th.baseUrl
                        , imageLoader = th.imageLoader
                        }
                    , Cmd.batch
                        [ Cmd.map FullMsg <| Maybe.withDefault Cmd.none <| Maybe.map toCmd progCmd
                        , getImgPosition
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        Prev ->
            updatePrevNext model shiftLeft

        Next ->
            updatePrevNext model shiftRight

        BackToThumbs ->
            case model of
                FullImage fi ->
                    let
                        scrollCmd =
                            case fi.scroll of
                                Nothing ->
                                    Cmd.none

                                Just pos ->
                                    Task.attempt (always NoUpdate) <| setViewportOf rootDivId 0 pos

                        th =
                            { album = baseAlbumOf <| FullImage fi
                            , vpInfo = fi.vpInfo
                            , baseUrl = fi.baseUrl
                            , imageLoader = fi.imageLoader
                            }

                        revisePending _ =
                            urlsToGet <| Thumbs th

                        ( newLoader, loadingCmd ) =
                            updatePending th.imageLoader revisePending
                    in
                    ( Thumbs { th | imageLoader = newLoader }
                    , Cmd.batch [ scrollCmd, loadingCmd ]
                    )

                _ ->
                    ( model, Cmd.none )

        TouchDragStart evt ->
            case model of
                FullImage fi ->
                    ( FullImage { fi | touchState = TU.update fi.touchState evt }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragContinue evt ->
            case model of
                FullImage fi ->
                    ( FullImage { fi | touchState = TU.update fi.touchState evt }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragAbandon ->
            case model of
                FullImage fi ->
                    ( FullImage { fi | touchState = TU.init }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchEndZoom oldState ->
            case model of
                FullImage fi ->
                    ( FullImage { fi | touchState = TU.endZoom oldState }, Cmd.none )

                Thumbs _ ->
                    ( model, Cmd.none )

        ImgPositionFailed ->
            case model of
                Thumbs _ ->
                    ( model, Cmd.none )

                FullImage _ ->
                    ( model, getImgPosition )

        GotImgPosition element ->
            case model of
                FullImage fi ->
                    ( FullImage { fi | imgPosition = Just element }, Cmd.none )

                Thumbs _ ->
                    ( model, Cmd.none )

        FullMsg progImgMsg ->
            case model of
                FullImage fi ->
                    let
                        ( newProgModel, newProgCmd, _ ) =
                            ProgressiveImage.update progImgMsg fi.progModel
                    in
                    ( FullImage { fi | progModel = newProgModel }, Cmd.map FullMsg newProgCmd )

                _ ->
                    ( model, Cmd.none )

        LoadingMsg lmsg ->
            let
                revisePending _ =
                    urlsToGet model
            in
            case model of
                Thumbs t ->
                    let
                        ( newLoadingModel, newLoadingCmd ) =
                            updateMany lmsg t.imageLoader revisePending
                    in
                    ( Thumbs { t | imageLoader = newLoadingModel }, newLoadingCmd )

                FullImage fi ->
                    let
                        ( newLoadingModel, newLoadingCmd ) =
                            updateMany lmsg fi.imageLoader revisePending
                    in
                    ( FullImage { fi | imageLoader = newLoadingModel }, newLoadingCmd )

        ThumbLoaded url ->
            case model of
                Thumbs t ->
                    ( Thumbs { t | imageLoader = markOne t.imageLoader url }, Cmd.none )

                FullImage fi ->
                    ( FullImage { fi | imageLoader = markOne fi.imageLoader url }, Cmd.none )

        NoUpdate ->
            ( model, Cmd.none )


baseAlbumOf : AlbumPage -> Album
baseAlbumOf ap =
    case ap of
        Thumbs t ->
            t.album

        FullImage fi ->
            let
                ( newFirst, newRest ) =
                    shiftToBeginning fi.prevImgs fi.album.imageFirst fi.album.imageRest
            in
            { title = fi.album.title
            , imageFirst = newFirst
            , imageRest = newRest
            , thumbnail = fi.album.thumbnail
            }


getImgPosition =
    Task.attempt (either (\_ -> ImgPositionFailed) GotImgPosition) <| getElement theImageId


progInit : Viewport -> Image -> Int -> Int -> ( ProgressiveImageModel, Maybe ProgressiveImageMsg )
progInit viewport i w h =
    let
        ( _, thumbWidth ) =
            colsWidth viewport

        smBiggerThan wMax hMax =
            smallestImageBiggerThan wMax hMax i.srcSetFirst i.srcSetRest
    in
    ProgressiveImage.init
        { mainImg = smBiggerThan w h
        , fallback = smBiggerThan 1 1
        , possiblyCached = [ smBiggerThan thumbWidth 1 ]
        , width = w
        , height = h
        }


updatePrevNext : AlbumPage -> (List Image -> Image -> List Image -> ( List Image, Image, List Image )) -> ( AlbumPage, Cmd AlbumPageMsg )
updatePrevNext model shifter =
    case model of
        FullImage fi ->
            let
                ( newPrev, newCur, newRest ) =
                    shifter fi.prevImgs fi.album.imageFirst fi.album.imageRest

                ( newProgModel, newCmd ) =
                    if fi.album.imageFirst == newCur then
                        ( fi.progModel, Nothing )

                    else
                        let
                            ( w, h ) =
                                fitImage
                                    newCur.srcSetFirst
                                    (floor fi.vpInfo.bodyViewport.viewport.width)
                                    (floor fi.vpInfo.bodyViewport.viewport.height)
                        in
                        progInit fi.vpInfo.bodyViewport newCur w h
            in
            ( FullImage
                { fi
                    | prevImgs = newPrev
                    , album =
                        { title = fi.album.title
                        , imageFirst = newCur
                        , imageRest = newRest
                        , thumbnail = fi.album.thumbnail
                        }
                    , progModel = newProgModel
                    , touchState = TU.init
                }
            , Cmd.batch
                [ Cmd.map FullMsg <| Maybe.withDefault Cmd.none <| Maybe.map toCmd newCmd
                , getImgPosition
                ]
            )

        _ ->
            ( model, Cmd.none )


urlsToGet : AlbumPage -> List Url
urlsToGet albumPage =
    case albumPage of
        Thumbs th ->
            ThumbPage.urlsToGet <| thumbModel th

        _ ->
            []


thumbModel th =
    { album = th.album
    , parents = []
    , bodyViewport = th.vpInfo.bodyViewport
    , rootDivViewport = th.vpInfo.rootDivViewport
    , imageLoader = th.imageLoader
    , baseUrl = th.baseUrl
    }


titleOf : AlbumPage -> String
titleOf albumPage =
    case albumPage of
        Thumbs th ->
            th.album.title

        FullImage fi ->
            fi.album.imageFirst.altText


view : AlbumPage -> AnchorFunction msg -> (Viewport -> msg) -> (AlbumList -> msg) -> (AlbumPageMsg -> msg) -> List AlbumList -> MainAlbumFlags -> Html msg
view albumPage a scrollMsgMaker showList wrapMsg parents flags =
    case albumPage of
        Thumbs th ->
            ThumbPage.view
                a
                scrollMsgMaker
                (\x -> \y -> \z -> wrapMsg (View x y z))
                (ThumbLoaded >> wrapMsg)
                showList
                { album = th.album
                , parents = parents
                , bodyViewport = th.vpInfo.bodyViewport
                , rootDivViewport = th.vpInfo.rootDivViewport
                , imageLoader = th.imageLoader
                , baseUrl = th.baseUrl
                }
                flags

        FullImage fi ->
            FullImagePage.view
                a
                { prevMsg = wrapMsg Prev
                , nextMsg = wrapMsg Next
                , backToThumbsMsg = wrapMsg BackToThumbs
                , showList = showList
                }
                { touchStartMsg = wrapMsg << TouchDragStart
                , touchContinueMsg = wrapMsg << TouchDragContinue
                , touchPrevNextMsg = wrapMsg << touchPrevNext fi.touchState
                }
                (wrapMsg << FullMsg)
                { prevImgs = fi.prevImgs
                , album = fi.album
                , viewport = fi.vpInfo.bodyViewport
                , progImgModel = fi.progModel
                , offset = getOffset fi.touchState
                , imgPosition = fi.imgPosition
                }
                parents
                flags


minDragLen : number
minDragLen =
    -- a bit of experimenting and a bit of HIG googling says ...
    75


touchPrevNext : TouchState -> Event -> AlbumPageMsg
touchPrevNext touchState _ =
    case TU.getOffset touchState of
        NoOffset ->
            NoUpdate

        Swipe distance direction ->
            if abs distance > minDragLen then
                case direction of
                    Left ->
                        Next

                    Right ->
                        Prev

            else
                TouchDragAbandon

        Zoom _ ->
            TouchEndZoom touchState


subscriptions : AlbumPage -> (AlbumPageMsg -> msg) -> msg -> Sub msg
subscriptions albumPage wrapper showParent =
    case albumPage of
        Thumbs t ->
            Sub.batch
                [ onEscape showParent <| wrapper NoUpdate
                , Sub.map wrapper <| subscriptionsMany t.imageLoader
                ]

        FullImage fi ->
            Sub.map wrapper <|
                Sub.batch <|
                    [ Sub.map FullMsg <| ProgressiveImage.subscriptions fi.progModel
                    , subscriptionsMany fi.imageLoader
                    , onKeyDown <|
                        Json.Decode.map
                            (\k ->
                                case k of
                                    "ArrowRight" ->
                                        Next

                                    "ArrowLeft" ->
                                        Prev

                                    "Escape" ->
                                        BackToThumbs

                                    _ ->
                                        NoUpdate
                            )
                        <|
                            field "key" string
                    ]


cmdFor : AlbumPage -> Cmd AlbumPageMsg
cmdFor albumPage =
    case albumPage of
        Thumbs t ->
            cmdForMany t.imageLoader

        FullImage fi ->
            cmdForMany fi.imageLoader


pageSize : AlbumPage -> ViewportInfo
pageSize albumPage =
    case albumPage of
        Thumbs th ->
            th.vpInfo

        FullImage fi ->
            fi.vpInfo


hashForAlbum : AlbumPage -> List AlbumList -> String
hashForAlbum albumPage parents =
    let
        titles =
            case albumPage of
                Thumbs th ->
                    [ th.album.title ]

                FullImage fi ->
                    [ fi.album.title, fi.album.imageFirst.altText ]
    in
    hashFromAlbumPath titles parents


eqIgnoringVpInfo : AlbumPage -> AlbumPage -> Bool
eqIgnoringVpInfo aPage1 aPage2 =
    case aPage1 of
        Thumbs th1 ->
            case aPage2 of
                Thumbs th2 ->
                    { th2 | vpInfo = th1.vpInfo } == th1

                FullImage _ ->
                    False

        FullImage fi1 ->
            case aPage2 of
                Thumbs _ ->
                    False

                FullImage fi2 ->
                    { fi2 | vpInfo = fi1.vpInfo }
                        == fi1
