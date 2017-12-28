module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), resetUrls, subscriptions, update, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import FullImagePage exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import ListUtils exposing (..)
import Set exposing (..)
import ThumbPage exposing (..)
import TouchEvents exposing (..)
import WinSize exposing (..)


type AlbumPage
    = Thumbs Album WinSize (Set String) (Set String)
    | FullImage (List Image) Album WinSize (Maybe ( Touch, Touch ))


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | TouchDragStart Touch
    | TouchDragContinue Touch
    | TouchDragAbandon
    | Prev
    | Next
    | BackToThumbs
    | NoUpdate



--note: no commands here


update : AlbumPageMsg -> AlbumPage -> AlbumPage
update msg model =
    case msg of
        View prevImgs curImg nextImgs ->
            case model of
                Thumbs album winSize _ _ ->
                    FullImage
                        prevImgs
                        { title = album.title
                        , imageFirst = curImg
                        , imageRest = nextImgs
                        , thumbnail = album.thumbnail
                        }
                        winSize
                        Nothing

                _ ->
                    model

        Prev ->
            case model of
                FullImage prevImgs album winSize _ ->
                    let
                        ( newPrev, newCur, newRest ) =
                            shiftLeft prevImgs album.imageFirst album.imageRest
                    in
                    FullImage
                        newPrev
                        { title = album.title
                        , imageFirst = newCur
                        , imageRest = newRest
                        , thumbnail = album.thumbnail
                        }
                        winSize
                        Nothing

                _ ->
                    model

        Next ->
            case model of
                FullImage prevImgs album winSize _ ->
                    let
                        ( newPrev, newCur, newRest ) =
                            shiftRight prevImgs album.imageFirst album.imageRest
                    in
                    FullImage
                        newPrev
                        { title = album.title
                        , imageFirst = newCur
                        , imageRest = newRest
                        , thumbnail = album.thumbnail
                        }
                        winSize
                        Nothing

                _ ->
                    model

        BackToThumbs ->
            case model of
                FullImage prevImgs album winSize _ ->
                    let
                        ( newFirst, newRest ) =
                            shiftToBeginning prevImgs album.imageFirst album.imageRest
                    in
                    Thumbs
                        { title = album.title
                        , imageFirst = newFirst
                        , imageRest = newRest
                        , thumbnail = album.thumbnail
                        }
                        winSize
                        empty
                        empty

                _ ->
                    model

        TouchDragStart pos ->
            case model of
                FullImage prevImgs album winSize dragInfo ->
                    FullImage prevImgs album winSize (Just ( pos, pos ))

                _ ->
                    model

        TouchDragContinue pos ->
            case model of
                FullImage prevImgs album winSize dragInfo ->
                    case dragInfo of
                        Nothing ->
                            FullImage prevImgs album winSize (Just ( pos, pos ))

                        Just ( start, cur ) ->
                            FullImage prevImgs album winSize (Just ( start, pos ))

                _ ->
                    model

        TouchDragAbandon ->
            case model of
                FullImage prevImgs album winSize _ ->
                    FullImage prevImgs album winSize Nothing

                _ ->
                    model

        NoUpdate ->
            model


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


view : AlbumPage -> (AlbumTreeNode -> msg) -> (AlbumPageMsg -> msg) -> List AlbumTreeNode -> AlbumBootstrapFlags -> Html msg
view albumPage showNode wrapMsg parents flags =
    case albumPage of
        Thumbs album winSize justLoadedImages readyToDisplayImages ->
            ThumbPage.view
                (\x -> \y -> \z -> wrapMsg (View x y z))
                showNode
                { album = album
                , parents = parents
                , winSize = winSize
                , justLoadedImages = justLoadedImages
                , readyToDisplayImages = readyToDisplayImages
                }
                flags

        FullImage prevImgs album winSize dragInfo ->
            Html.map wrapMsg <|
                FullImagePage.view
                    Prev
                    Next
                    BackToThumbs
                    TouchDragStart
                    TouchDragContinue
                    (touchPrevNext dragInfo)
                    NoUpdate
                    { prevImgs = prevImgs
                    , album = album
                    , winSize = winSize
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


subscriptions : AlbumPage -> Sub AlbumPageMsg
subscriptions albumPage =
    case albumPage of
        Thumbs _ _ _ _ ->
            Sub.none

        FullImage _ _ _ _ ->
            downs
                (\keycode ->
                    case keycode of
                        39 ->
                            -- right arrow
                            Next

                        37 ->
                            -- left arrow
                            Prev

                        38 ->
                            -- up arrow
                            BackToThumbs

                        _ ->
                            NoUpdate
                )
