import Album exposing (..)

import Html exposing (..)

type AlbumPage
    = Thumbs Album WinSize
    | FullImage Album Int

type AlbumPageMsg
    = View Int
    | Prev
    | Next
    | BackToThumbs

view : AlbumPage -> Html AlbumPageMsg
view albumPage =
    case albumPage of
        Thumbs album winSize ->
            ThumbPage.view View { album : album, winSize : winSize }

        FullImage album index ->
            FullImagePage.view Prev Next BackToThumbs { album : album, index : index }

