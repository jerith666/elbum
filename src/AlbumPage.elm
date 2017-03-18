module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), view, update, subscriptions)

import ListUtils exposing (..)
import WinSize exposing (..)
import Album exposing (..)
import ThumbPage exposing (..)
import FullImagePage exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import TouchEvents exposing (..)


type Pos
    = Pos Int Int


type AlbumPage
    = Thumbs Album WinSize
    | FullImage (List Image) Album WinSize (Maybe ( Pos, Pos ))


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | TouchDragStart Pos
    | TouchDragContinue Pos
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
                            }
                            winSize

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

        NoUpdate ->
            model


view : AlbumPage -> Html AlbumPageMsg
view albumPage =
    case albumPage of
        Thumbs album winSize ->
            ThumbPage.view View { album = album, winSize = winSize }

        FullImage prevImgs album winSize dragInfo ->
            FullImagePage.view
                Prev
                Next
                BackToThumbs
                NoUpdate
                { prevImgs = prevImgs
                , album = album
                , winSize = winSize
                , offset = offsetFor dragInfo
                }


offsetFor : Maybe ( Pos, Pos ) -> ( Int, Int )
offsetFor dragInfo =
    case dragInfo of
        Nothing ->
            ( 0, 0 )

        Just ( Pos sx sy, Pos cx cy ) ->
            ( cx - sx, cy - sy )


subscriptions : AlbumPage -> Sub AlbumPageMsg
subscriptions albumPage =
    case albumPage of
        Thumbs _ _ ->
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
