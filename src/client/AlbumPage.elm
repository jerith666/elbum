module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), ThumbLoadState(..), ViewportInfo, eqIgnoringVpInfo, getImgPosition, hashForAlbum, pageSize, progInit, resetUrls, subscriptions, titleOf, update, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import FullImagePage exposing (..)
import Html.Events.Extra.Touch exposing (..)
import Html.Styled exposing (..)
import ImageViews exposing (..)
import Json.Decode exposing (..)
import ProgressiveImage exposing (..)
import Set exposing (..)
import Task exposing (..)
import ThumbPage exposing (..)
import Utils.AlbumUtils exposing (..)
import Utils.KeyboardUtils exposing (onEscape)
import Utils.ListUtils exposing (..)
import Utils.ResultUtils exposing (..)
import Utils.TouchUtils as TU exposing (..)


type AlbumPage
    = Thumbs
        { album : Album
        , vpInfo : ViewportInfo
        , justLoadedImages : Set String
        , readyToDisplayImages : Set String
        }
    | FullImage
        { prevImgs : List Image
        , album : Album
        , progModel : ProgressiveImageModel
        , vpInfo : ViewportInfo
        , scroll : Maybe Float
        , touchState : TouchState
        , imgPosition : Maybe Element
        , thumbLoadState : ThumbLoadState
        }


type ThumbLoadState
    = SomeMissing
    | AllLoaded


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
    | ImgPositionFailed Browser.Dom.Error
    | GotImgPosition Element
    | NoUpdate


update : AlbumPageMsg -> AlbumPage -> Maybe Float -> ( AlbumPage, Cmd AlbumPageMsg )
update msg model scroll =
    case msg of
        View prevImgs curImg nextImgs ->
            case model of
                Thumbs th ->
                    let
                        ( w, h ) =
                            fitImage curImg.srcSetFirst (floor th.vpInfo.bodyViewport.viewport.width) (floor th.vpInfo.bodyViewport.viewport.height)

                        ( progModel, progCmd ) =
                            progInit th.vpInfo.bodyViewport curImg w h

                        imgCount =
                            List.length prevImgs + 1 + List.length nextImgs

                        thumbLoadState =
                            case imgCount == Set.size th.readyToDisplayImages of
                                True ->
                                    AllLoaded

                                False ->
                                    SomeMissing
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
                        , thumbLoadState = thumbLoadState
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
                        ( newFirst, newRest ) =
                            shiftToBeginning fi.prevImgs fi.album.imageFirst fi.album.imageRest

                        scrollCmd =
                            case fi.scroll of
                                Nothing ->
                                    Cmd.none

                                Just pos ->
                                    Task.attempt (\_ -> NoUpdate) <| setViewportOf rootDivId 0 pos

                        th =
                            { album =
                                { title = fi.album.title
                                , imageFirst = newFirst
                                , imageRest = newRest
                                , thumbnail = fi.album.thumbnail
                                }
                            , vpInfo = fi.vpInfo
                            , justLoadedImages = empty
                            , readyToDisplayImages = empty
                            }

                        readyToDisplayImages =
                            case fi.thumbLoadState of
                                SomeMissing ->
                                    empty

                                AllLoaded ->
                                    allUrls <| thumbModel th
                    in
                    ( Thumbs { th | readyToDisplayImages = readyToDisplayImages }
                    , scrollCmd
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

        ImgPositionFailed err ->
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
                        ( newProgModel, newProgCmd ) =
                            ProgressiveImage.update progImgMsg fi.progModel
                    in
                    ( FullImage { fi | progModel = newProgModel }, Cmd.map FullMsg newProgCmd )

                _ ->
                    ( model, Cmd.none )

        NoUpdate ->
            ( model, Cmd.none )


getImgPosition =
    Task.attempt (either ImgPositionFailed GotImgPosition) <| getElement theImageId


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
                                fitImage newCur.srcSetFirst (floor fi.vpInfo.bodyViewport.viewport.width) (floor fi.vpInfo.bodyViewport.viewport.height)
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


resetUrls : AlbumPageMsg -> Bool
resetUrls msg =
    case msg of
        BackToThumbs ->
            True

        View _ _ _ ->
            True

        _ ->
            False


urlsToGet : AlbumPage -> Set String
urlsToGet albumPage =
    case albumPage of
        Thumbs th ->
            ThumbPage.urlsToGet <| thumbModel th

        _ ->
            empty


thumbModel th =
    { album = th.album
    , parents = []
    , bodyViewport = th.vpInfo.bodyViewport
    , rootDivViewport = th.vpInfo.rootDivViewport
    , justLoadedImages = th.justLoadedImages
    , readyToDisplayImages = th.readyToDisplayImages
    }


titleOf : AlbumPage -> String
titleOf albumPage =
    case albumPage of
        Thumbs th ->
            th.album.title

        FullImage fi ->
            fi.album.imageFirst.altText


view : AlbumPage -> (Viewport -> msg) -> (AlbumList -> msg) -> (AlbumPageMsg -> msg) -> List AlbumList -> AlbumBootstrapFlags -> Html msg
view albumPage scrollMsgMaker showList wrapMsg parents flags =
    case albumPage of
        Thumbs th ->
            ThumbPage.view
                scrollMsgMaker
                (\x -> \y -> \z -> wrapMsg (View x y z))
                showList
                { album = th.album
                , parents = parents
                , bodyViewport = th.vpInfo.bodyViewport
                , rootDivViewport = th.vpInfo.rootDivViewport
                , justLoadedImages = th.justLoadedImages
                , readyToDisplayImages = th.readyToDisplayImages
                }
                flags

        FullImage fi ->
            FullImagePage.view
                { prevMsg = wrapMsg Prev
                , nextMsg = wrapMsg Next
                , backToThumbsMsg = wrapMsg BackToThumbs
                , showList = showList
                }
                { touchStartMsg = wrapMsg << TouchDragStart
                , touchContinueMsg = wrapMsg << TouchDragContinue
                , touchPrevNextMsg = wrapMsg << touchPrevNext fi.touchState
                }
                (wrapMsg NoUpdate)
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

        Zoom zoom ->
            TouchEndZoom touchState


subscriptions : AlbumPage -> (AlbumPageMsg -> msg) -> msg -> Sub msg
subscriptions albumPage wrapper showParent =
    case albumPage of
        Thumbs _ ->
            onEscape showParent <| wrapper NoUpdate

        FullImage fi ->
            Sub.batch
                [ Sub.map wrapper <| Sub.map FullMsg <| ProgressiveImage.subscriptions fi.progModel
                , Sub.map wrapper <|
                    onKeyDown <|
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
                    th1.album == th2.album && th1.justLoadedImages == th2.justLoadedImages && th1.readyToDisplayImages == th2.readyToDisplayImages

                FullImage fi ->
                    False

        FullImage fi1 ->
            case aPage2 of
                Thumbs _ ->
                    False

                FullImage fi2 ->
                    fi1.prevImgs
                        == fi2.prevImgs
                        && fi1.album
                        == fi2.album
                        && fi1.progModel
                        == fi2.progModel
                        && fi1.scroll
                        == fi2.scroll
                        && fi1.touchState
                        == fi2.touchState
                        && fi1.imgPosition
                        == fi2.imgPosition
