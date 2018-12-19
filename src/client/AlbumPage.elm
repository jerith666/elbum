module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), ViewportInfo, eqIgnoringVpInfo, hashForAlbum, pageSize, progInit, resetUrls, subscriptions, titleOf, update, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import AlbumUtils exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import FullImagePage exposing (..)
import Html.Styled exposing (..)
import ImageViews exposing (..)
import Json.Decode exposing (..)
import KeyboardUtils exposing (onEscape)
import ListUtils exposing (..)
import ProgressiveImage exposing (..)
import ResultUtils exposing (..)
import Set exposing (..)
import Task exposing (..)
import ThumbPage exposing (..)
import TouchEvents exposing (..)


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
        , dragInfo : Maybe ( Touch, Touch )
        }


type alias ViewportInfo =
    { bodyViewport : Viewport, rootDivViewport : Maybe Viewport }


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | TouchDragStart Touch
    | TouchDragContinue Touch
    | TouchDragAbandon
    | Prev
    | Next
    | BackToThumbs
    | FullMsg ProgressiveImageMsg
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
                        , dragInfo = Nothing
                        }
                    , Cmd.map FullMsg <| Maybe.withDefault Cmd.none <| Maybe.map toCmd progCmd
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
                    in
                    ( Thumbs
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
                    , scrollCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TouchDragStart pos ->
            case model of
                FullImage fi ->
                    ( FullImage { fi | dragInfo = Just ( pos, pos ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragContinue pos ->
            case model of
                FullImage fi ->
                    case fi.dragInfo of
                        Nothing ->
                            ( FullImage { fi | dragInfo = Just ( pos, pos ) }, Cmd.none )

                        Just ( start, cur ) ->
                            ( FullImage { fi | dragInfo = Just ( start, pos ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragAbandon ->
            case model of
                FullImage fi ->
                    ( FullImage { fi | dragInfo = Nothing }, Cmd.none )

                _ ->
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
                    , dragInfo = Nothing
                }
            , Cmd.map FullMsg <| Maybe.withDefault Cmd.none <| Maybe.map toCmd newCmd
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
            ThumbPage.urlsToGet
                { album = th.album
                , parents = []
                , bodyViewport = th.vpInfo.bodyViewport
                , rootDivViewport = th.vpInfo.rootDivViewport
                , justLoadedImages = th.justLoadedImages
                , readyToDisplayImages = th.readyToDisplayImages
                }

        _ ->
            empty


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
                , touchPrevNextMsg = wrapMsg << touchPrevNext fi.dragInfo
                }
                (wrapMsg NoUpdate)
                (wrapMsg << FullMsg)
                { prevImgs = fi.prevImgs
                , album = fi.album
                , viewport = fi.vpInfo.bodyViewport
                , progImgModel = fi.progModel
                , offset = offsetFor fi.dragInfo
                }
                parents
                flags


minDragLen : number
minDragLen =
    -- a bit of experimenting and a bit of HIG googling says ...
    75


touchPrevNext : Maybe ( Touch, Touch ) -> Touch -> AlbumPageMsg
touchPrevNext dragInfo touch =
    case dragInfo of
        Nothing ->
            NoUpdate

        Just ( start, cur ) ->
            if abs (start.clientX - touch.clientX) > minDragLen then
                case getDirectionX start.clientX touch.clientX of
                    Left ->
                        Next

                    Right ->
                        Prev

                    _ ->
                        TouchDragAbandon

            else
                TouchDragAbandon


offsetFor : Maybe ( Touch, Touch ) -> ( Float, Float )
offsetFor dragInfo =
    case dragInfo of
        Nothing ->
            ( 0, 0 )

        Just ( start, current ) ->
            ( current.clientX - start.clientX, current.clientY - start.clientY )


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
                    fi1.prevImgs == fi2.prevImgs && fi1.album == fi2.album && fi1.progModel == fi2.progModel && fi1.scroll == fi2.scroll && fi1.dragInfo == fi2.dragInfo
