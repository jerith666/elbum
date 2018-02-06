module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), progInit, resetUrls, subscriptions, update, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import FullImagePage exposing (..)
import Html.Styled exposing (..)
import ImageViews exposing (..)
import Keyboard exposing (..)
import KeyboardUtils exposing (onEscape)
import ListUtils exposing (..)
import ProgressiveImage exposing (..)
import Set exposing (..)
import ThumbPage exposing (..)
import TouchEvents exposing (..)
import WinSize exposing (..)


type AlbumPage
    = Thumbs Album WinSize (Set String) (Set String)
    | FullImage (List Image) Album ProgressiveImageModel WinSize (Maybe ( Touch, Touch ))


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


update : AlbumPageMsg -> AlbumPage -> ( AlbumPage, Cmd AlbumPageMsg )
update msg model =
    case msg of
        View prevImgs curImg nextImgs ->
            case model of
                Thumbs album winSize _ _ ->
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
                FullImage prevImgs album _ winSize _ ->
                    let
                        ( newFirst, newRest ) =
                            shiftToBeginning prevImgs album.imageFirst album.imageRest
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
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TouchDragStart pos ->
            case model of
                FullImage prevImgs album progModel winSize dragInfo ->
                    ( FullImage prevImgs album progModel winSize (Just ( pos, pos )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragContinue pos ->
            case model of
                FullImage prevImgs album progModel winSize dragInfo ->
                    case dragInfo of
                        Nothing ->
                            ( FullImage prevImgs album progModel winSize (Just ( pos, pos )), Cmd.none )

                        Just ( start, cur ) ->
                            ( FullImage prevImgs album progModel winSize (Just ( start, pos )), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TouchDragAbandon ->
            case model of
                FullImage prevImgs album progModel winSize _ ->
                    ( FullImage prevImgs album progModel winSize Nothing, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FullMsg progImgMsg ->
            case model of
                FullImage prevImgs album progModel winSize dragInfo ->
                    let
                        ( newProgModel, newProgCmd ) =
                            ProgressiveImage.update progImgMsg progModel
                    in
                    ( FullImage prevImgs album newProgModel winSize dragInfo, Cmd.map FullMsg newProgCmd )

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
        FullImage prevImgs album oldProgModel winSize _ ->
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


view : AlbumPage -> (AlbumList -> msg) -> (AlbumPageMsg -> msg) -> List AlbumList -> AlbumBootstrapFlags -> Html msg
view albumPage showList wrapMsg parents flags =
    case albumPage of
        Thumbs album winSize justLoadedImages readyToDisplayImages ->
            ThumbPage.view
                (\x -> \y -> \z -> wrapMsg (View x y z))
                showList
                { album = album
                , parents = parents
                , winSize = winSize
                , justLoadedImages = justLoadedImages
                , readyToDisplayImages = readyToDisplayImages
                }
                flags

        FullImage prevImgs album progModel winSize dragInfo ->
            Html.Styled.map wrapMsg <|
                FullImagePage.view
                    { prevMsg = Prev, nextMsg = Next, backToThumbsMsg = BackToThumbs }
                    { touchStartMsg = TouchDragStart
                    , touchContinueMsg = TouchDragContinue
                    , touchPrevNextMsg = touchPrevNext dragInfo
                    }
                    NoUpdate
                    FullMsg
                    { prevImgs = prevImgs
                    , album = album
                    , winSize = winSize
                    , progImgModel = progModel
                    , offset = offsetFor dragInfo
                    }
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
        Thumbs _ _ _ _ ->
            onEscape showParent <| wrapper NoUpdate

        FullImage _ _ progImgModel _ _ ->
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
