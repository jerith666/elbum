module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), progInit, resetUrls, subscriptions, titleOf, update, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Dom.Scroll exposing (..)
import FullImagePage exposing (..)
import Html.Styled exposing (..)
import ImageViews exposing (..)
import Keyboard exposing (..)
import KeyboardUtils exposing (onEscape)
import ListUtils exposing (..)
import ProgressiveImage exposing (..)
import Set exposing (..)
import Task exposing (..)
import ThumbPage exposing (..)
import TouchEvents exposing (..)
import WinSize exposing (..)


type AlbumPage
    = Thumbs Album WinSize (Set String) (Set String)
    | GettingScroll Album (List Image) Image (List Image) WinSize AlbumPage
    | FullImage (List Image) Album ProgressiveImageModel WinSize (Maybe Float) (Maybe ( Touch, Touch ))


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | GotScroll (Maybe Float)
    | TouchDragStart Touch
    | TouchDragContinue Touch
    | TouchDragAbandon
    | Prev
    | Next
    | BackToThumbs
    | FullMsg ProgressiveImageMsg
    | NoUpdate


update : AlbumPageMsg -> AlbumPage -> ( AlbumPage, Cmd AlbumPageMsg )
update msg model =
    case msg of
        View prevImgs curImg nextImgs ->
            case model of
                Thumbs album winSize _ _ ->
                    ( GettingScroll album prevImgs curImg nextImgs winSize model
                    , attempt (GotScroll << Result.toMaybe) <| y rootDivId
                    )

                _ ->
                    ( model, Cmd.none )

        GotScroll scroll ->
            case model of
                GettingScroll album prevImgs curImg nextImgs winSize underlyingModel ->
                    let
                        ( w, h ) =
                            fitImage curImg.srcSetFirst winSize.width winSize.height

                        ( progModel, progCmd ) =
                            progInit winSize curImg w h
                    in
                    ( FullImage
                        prevImgs
                        { title = album.title
                        , imageFirst = curImg
                        , imageRest = nextImgs
                        , thumbnail = album.thumbnail
                        }
                        progModel
                        winSize
                        scroll
                        Nothing
                    , Cmd.map FullMsg progCmd
                    )

                _ ->
                    ( model, Cmd.none )

        Prev ->
            updatePrevNext model shiftLeft

        Next ->
            updatePrevNext model shiftRight

        BackToThumbs ->
            case model of
                FullImage prevImgs album _ winSize savedScroll _ ->
                    let
                        ( newFirst, newRest ) =
                            shiftToBeginning prevImgs album.imageFirst album.imageRest

                        scrollCmd =
                            case savedScroll of
                                Nothing ->
                                    Cmd.none

                                Just pos ->
                                    Task.attempt (\_ -> NoUpdate) <| toY rootDivId pos
                    in
                    ( Thumbs
                        { title = album.title
                        , imageFirst = newFirst
                        , imageRest = newRest
                        , thumbnail = album.thumbnail
                        }
                        winSize
                        empty
                        empty
                    , scrollCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TouchDragStart pos ->
            case model of
                FullImage prevImgs album progModel winSize savedScroll dragInfo ->
                    ( FullImage prevImgs album progModel winSize savedScroll (Just ( pos, pos )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragContinue pos ->
            case model of
                FullImage prevImgs album progModel winSize savedScroll dragInfo ->
                    case dragInfo of
                        Nothing ->
                            ( FullImage prevImgs album progModel winSize savedScroll (Just ( pos, pos )), Cmd.none )

                        Just ( start, cur ) ->
                            ( FullImage prevImgs album progModel winSize savedScroll (Just ( start, pos )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragAbandon ->
            case model of
                FullImage prevImgs album progModel winSize savedScroll _ ->
                    ( FullImage prevImgs album progModel winSize savedScroll Nothing, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FullMsg progImgMsg ->
            case model of
                FullImage prevImgs album progModel winSize savedScroll dragInfo ->
                    let
                        ( newProgModel, newProgCmd ) =
                            ProgressiveImage.update progImgMsg progModel
                    in
                    ( FullImage prevImgs album newProgModel winSize savedScroll dragInfo, Cmd.map FullMsg newProgCmd )

                _ ->
                    ( model, Cmd.none )

        NoUpdate ->
            ( model, Cmd.none )


progInit : WinSize -> Image -> Int -> Int -> ( ProgressiveImageModel, Cmd ProgressiveImageMsg )
progInit winSize i w h =
    let
        ( _, thumbWidth ) =
            colsWidth winSize

        smBiggerThan w h =
            smallestImageBiggerThan w h i.srcSetFirst i.srcSetRest
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
        FullImage prevImgs album oldProgModel winSize savedScroll _ ->
            let
                ( newPrev, newCur, newRest ) =
                    shifter prevImgs album.imageFirst album.imageRest

                ( newProgModel, newCmd ) =
                    if album.imageFirst == newCur then
                        ( oldProgModel, Cmd.none )
                    else
                        let
                            ( w, h ) =
                                fitImage newCur.srcSetFirst winSize.width winSize.height
                        in
                        progInit winSize newCur w h
            in
            ( FullImage
                newPrev
                { title = album.title
                , imageFirst = newCur
                , imageRest = newRest
                , thumbnail = album.thumbnail
                }
                newProgModel
                winSize
                savedScroll
                Nothing
            , Cmd.map FullMsg newCmd
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
        Thumbs album winSize justLoadedImages readyToDisplayImages ->
            ThumbPage.urlsToGet
                { album = album
                , parents = []
                , winSize = winSize
                , justLoadedImages = justLoadedImages
                , readyToDisplayImages = readyToDisplayImages
                }

        _ ->
            empty


titleOf : AlbumPage -> String
titleOf albumPage =
    case albumPage of
        Thumbs album _ _ _ ->
            album.title

        GettingScroll album _ _ _ _ _ ->
            album.title

        FullImage _ album _ _ _ _ ->
            album.imageFirst.altText


view : AlbumPage -> (Float -> msg) -> (AlbumList -> msg) -> (AlbumPageMsg -> msg) -> List AlbumList -> AlbumBootstrapFlags -> Html msg
view albumPage scrollMsgMaker showList wrapMsg parents flags =
    case albumPage of
        GettingScroll _ _ _ _ _ underlyingModel ->
            view underlyingModel scrollMsgMaker showList wrapMsg parents flags

        Thumbs album winSize justLoadedImages readyToDisplayImages ->
            ThumbPage.view
                scrollMsgMaker
                (\x -> \y -> \z -> wrapMsg (View x y z))
                showList
                { album = album
                , parents = parents
                , winSize = winSize
                , justLoadedImages = justLoadedImages
                , readyToDisplayImages = readyToDisplayImages
                }
                flags

        FullImage prevImgs album progModel winSize _ dragInfo ->
            FullImagePage.view
                { prevMsg = wrapMsg Prev
                , nextMsg = wrapMsg Next
                , backToThumbsMsg = wrapMsg BackToThumbs
                , showList = showList
                }
                { touchStartMsg = wrapMsg << TouchDragStart
                , touchContinueMsg = wrapMsg << TouchDragContinue
                , touchPrevNextMsg = wrapMsg << touchPrevNext dragInfo
                , scrollMsgMaker = scrollMsgMaker
                }
                (wrapMsg NoUpdate)
                (wrapMsg << FullMsg)
                { prevImgs = prevImgs
                , album = album
                , winSize = winSize
                , progImgModel = progModel
                , offset = offsetFor dragInfo
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
        GettingScroll _ _ _ _ _ underlyingModel ->
            subscriptions underlyingModel wrapper showParent

        Thumbs _ _ _ _ ->
            onEscape showParent <| wrapper NoUpdate

        FullImage _ _ progImgModel _ _ _ ->
            Sub.batch
                [ Sub.map wrapper <| Sub.map FullMsg <| ProgressiveImage.subscriptions progImgModel
                , Sub.map wrapper <|
                    downs
                        (\keycode ->
                            case keycode of
                                39 ->
                                    -- right arrow
                                    Next

                                37 ->
                                    -- left arrow
                                    Prev

                                27 ->
                                    -- escape
                                    BackToThumbs

                                _ ->
                                    NoUpdate
                        )
                ]
