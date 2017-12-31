module AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..), resetUrls, subscriptions, update, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import FullImagePage exposing (..)
import Html exposing (..)
import Keyboard exposing (..)
import KeyboardUtils exposing (onUpArrow)
import ListUtils exposing (..)
import Set exposing (..)
import ThumbPage exposing (..)
import TouchEvents exposing (..)
import WinSize exposing (..)


type AlbumPage
    = Thumbs Album WinSize (Set String) (Set String)
    | FullImage (List Image) Album Bool WinSize (Maybe ( Touch, Touch ))


type AlbumPageMsg
    = View (List Image) Image (List Image)
    | TouchDragStart Touch
    | TouchDragContinue Touch
    | TouchDragAbandon
    | Loaded String
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
                        False
                        winSize
                        Nothing

                _ ->
                    model

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

        Loaded url ->
            case model of
                Thumbs _ _ _ _ ->
                    model

                FullImage prevImgs album bool winSize dragInfo ->
                    let
                        loaded =
                            url == album.imageFirst.srcSetFirst.url
                    in
                    FullImage prevImgs album loaded winSize dragInfo

        TouchDragStart pos ->
            case model of
                FullImage prevImgs album loaded winSize dragInfo ->
                    FullImage prevImgs album loaded winSize (Just ( pos, pos ))

                _ ->
                    model

        TouchDragContinue pos ->
            case model of
                FullImage prevImgs album loaded winSize dragInfo ->
                    case dragInfo of
                        Nothing ->
                            FullImage prevImgs album loaded winSize (Just ( pos, pos ))

                        Just ( start, cur ) ->
                            FullImage prevImgs album loaded winSize (Just ( start, pos ))

                _ ->
                    model

        TouchDragAbandon ->
            case model of
                FullImage prevImgs album loaded winSize _ ->
                    FullImage prevImgs album loaded winSize Nothing

                _ ->
                    model

        NoUpdate ->
            model


updatePrevNext : AlbumPage -> (List Image -> Image -> List Image -> ( List Image, Image, List Image )) -> AlbumPage
updatePrevNext model shifter =
    case model of
        FullImage prevImgs album oldLoaded winSize _ ->
            let
                ( newPrev, newCur, newRest ) =
                    shifter prevImgs album.imageFirst album.imageRest

                newLoaded =
                    if album.imageFirst == newCur then
                        oldLoaded
                    else
                        False
            in
            FullImage
                newPrev
                { title = album.title
                , imageFirst = newCur
                , imageRest = newRest
                , thumbnail = album.thumbnail
                }
                newLoaded
                winSize
                Nothing

        _ ->
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

        FullImage prevImgs album loaded winSize dragInfo ->
            Html.map wrapMsg <|
                FullImagePage.view
                    { prevMsg = Prev, nextMsg = Next, backToThumbsMsg = BackToThumbs }
                    Loaded
                    { touchStartMsg = TouchDragStart
                    , touchContinueMsg = TouchDragContinue
                    , touchPrevNextMsg =
                        touchPrevNext dragInfo
                    }
                    NoUpdate
                    { prevImgs = prevImgs
                    , album = album
                    , winSize = winSize
                    , offset = offsetFor dragInfo
                    , loaded = loaded
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
            onUpArrow showParent <| wrapper NoUpdate

        FullImage _ _ _ _ _ ->
            Sub.map wrapper <|
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
