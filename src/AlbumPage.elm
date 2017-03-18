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
                        (newPrev, newCur, newRest) = shiftLeft prevImgs album.imageFirst album.imageRest
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
                        (newPrev, newCur, newRest) = shiftRight prevImgs album.imageFirst album.imageRest
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

{-return a tuple with the new middle element taken from the beginning of the right hand list,
  the old middle element appended to the left hand list, if possible, otherwise do nothing-}
shiftRight : List a -> a -> List a -> (List a, a, List a)
shiftRight   xLefts    x    xRights =
    case xRights of
        [] ->
            (xLefts, x, xRights)
        xRight :: xRightRights ->
            (xLefts ++ [x], xRight, xRightRights)


{-return a tuple with the new middle element taken from the end of the left hand list,
  the old middle element prepended to the right hand list, if possible, otherwise do nothing-}
shiftLeft : List a -> a -> List a -> (List a, a, List a)
shiftLeft   xLefts    x    xRights =
    case xLefts of
        [] ->
            (xLefts, x, xRights)
        [xLeft] ->
            ([], xLeft, x :: xRights)
        xLeft :: xLeftRights ->
            let
                (xLRss, xss, xRss) = shiftLeft xLeftRights x xRights
            in
                (xLeft :: xLRss, xss, xRss)


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
