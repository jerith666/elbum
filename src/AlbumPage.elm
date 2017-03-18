module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), view, update, subscriptions)

import ListUtils exposing (..)
import WinSize exposing (..)
import Album exposing (..)
import ThumbPage exposing (..)
import FullImagePage exposing (..)
import Html exposing (..)
import Keyboard exposing (..)


type AlbumPage
    = Thumbs Album WinSize
    | FullImage (List Image) Album WinSize


type AlbumPageMsg
    = View (List Image) Image (List Image)
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
                Thumbs album winSize ->
                    FullImage
                        prevImgs
                        { title = album.title
                        , imageFirst = curImg
                        , imageRest = nextImgs
                        }
                        winSize

                _ ->
                    model

        Prev ->
            case model of
                FullImage prevImgs album winSize ->
                    let
                        ( newPrev, newCur, newRest ) =
                            shiftLeft prevImgs album.imageFirst album.imageRest
                    in
                        FullImage
                            newPrev
                            { title = album.title
                            , imageFirst = newCur
                            , imageRest = newRest
                            }
                            winSize

                _ ->
                    model

        Next ->
            case model of
                FullImage prevImgs album winSize ->
                    let
                        ( newPrev, newCur, newRest ) =
                            shiftRight prevImgs album.imageFirst album.imageRest
                    in
                        FullImage
                            newPrev
                            { title = album.title
                            , imageFirst = newCur
                            , imageRest = newRest
                            }
                            winSize

                _ ->
                    model

        BackToThumbs ->
            case model of
                Thumbs album winSize ->
                    model

                FullImage prevImgs album winSize ->
                    let
                        ( newFirst, newRest ) =
                            shiftToBeginning prevImgs album.imageFirst album.imageRest
                    in
                        Thumbs
                            { title = album.title
                            , imageFirst = newFirst
                            , imageRest = newRest
                            }
                            winSize

        NoUpdate ->
            model


view : AlbumPage -> Html AlbumPageMsg
view albumPage =
    case albumPage of
        Thumbs album winSize ->
            ThumbPage.view View { album = album, winSize = winSize }

        FullImage prevImgs album winSize ->
            FullImagePage.view
                Prev
                Next
                BackToThumbs
                (View prevImgs album.imageFirst album.imageRest)
                { prevImgs = prevImgs
                , album = album
                , winSize = winSize
                , offset = ( 0, 0 )
                }


subscriptions : AlbumPage -> Sub AlbumPageMsg
subscriptions albumPage =
    case albumPage of
        Thumbs _ _ ->
            Sub.none

        FullImage _ _ _ ->
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
