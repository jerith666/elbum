module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), view, update)

import WinSize exposing (..)
import Album exposing (..)
import ThumbPage exposing (..)
import FullImagePage exposing (..)
import Html exposing (..)


type AlbumPage
    = Thumbs Album WinSize
    | FullImage (List Image) Album WinSize


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | Prev
    | Next
    | BackToThumbs



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
                        (newPrev, newRest, newCur) = shift prevImgs album.imageRest album.imageFirst (\i -> \is -> i :: is)
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
                        (newRest, newPrev, newCur) = shift album.imageRest prevImgs album.imageFirst (\i -> \is -> is ++ [i])
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
                        (newFirst, newRest) = shiftToBeginning prevImgs album.imageFirst album.imageRest
                    in
                        Thumbs
                            { title = album.title
                            , imageFirst = newFirst
                            , imageRest = newRest
                            }
                            winSize

shiftToBeginning : List Image -> Image -> List Image -> (Image, List Image)
shiftToBeginning prevImgs img restImgs =
    case prevImgs of
        [] ->
            (img, restImgs)
        prev1 :: prevRest ->
            (prev1, prevRest ++ (img :: restImgs))

--TODO generic shift is too tricky, probably simpler to just have a shiftFwd and shiftRev
shift : List Image -> List Image -> Image -> (Image -> List Image -> List Image) -> (List Image, List Image, Image)
shift   takeFromImgs  addToImgs     oldImg   combiner                            =
    case takeFromImgs of
        [] ->
            (takeFromImgs, addToImgs, oldImg)
        take1 :: takeRest ->
            (takeRest, combiner oldImg addToImgs, take1)

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
                , offset = (0,0)
                }
