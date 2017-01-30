module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), view, update)

import WinSize exposing (..)
import Album exposing (..)
import ThumbPage exposing (..)
import FullImagePage exposing (..)
import Html exposing (..)


type AlbumPage
    = Thumbs Album WinSize
    | FullImage Album Int WinSize


type AlbumPageMsg
    = View Int
    | Prev
    | Next
    | BackToThumbs



--note: no commands here


update : AlbumPageMsg -> AlbumPage -> AlbumPage
update msg model =
    case msg of
        View index ->
            case model of
                Thumbs album winSize ->
                    FullImage album (bounded album index) winSize

                _ ->
                    model

        Prev ->
            case model of
                FullImage album index winSize ->
                    FullImage album (bounded album (index - 1)) winSize

                _ ->
                    model

        Next ->
            case model of
                FullImage album index winSize ->
                    FullImage album (bounded album (index + 1)) winSize

                _ ->
                    model

        BackToThumbs ->
            case model of
                Thumbs album winSize ->
                    model

                FullImage album index winSize ->
                    Thumbs album winSize


bounded : Album -> Int -> Int
bounded album proposedIndex =
    Basics.min
        (Basics.max proposedIndex 0)
        (List.length album.images - 1)


view : AlbumPage -> Html AlbumPageMsg
view albumPage =
    case albumPage of
        Thumbs album winSize ->
            ThumbPage.view View { album = album, winSize = winSize }

        FullImage album index winSize ->
            FullImagePage.view Prev Next BackToThumbs (View index) { album = album, index = index, winSize = winSize, offset = (0,0) }
