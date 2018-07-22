module AlbumListPage exposing (AlbumListPage(..), view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import ImageViews exposing (..)
import ThumbPage exposing (albumTitle, viewThumb)
import WinSize exposing (..)


type AlbumListPage
    = AlbumListPage AlbumList WinSize (List ( AlbumList, Maybe Float ))


view : AlbumListPage -> (AlbumList -> msg) -> (Album -> msg) -> (Float -> msg) -> AlbumBootstrapFlags -> Html msg
view (AlbumListPage albumList winSize parents) viewList viewAlbum scrollMsgMaker flags =
    rootDivFlex
        flags
        column
        scrollMsgMaker
        []
    <|
        [ albumTitle albumList.listTitle (List.map Tuple.first parents) viewList [] [] ]
            ++ (List.reverse <|
                    [ viewAlbumOrList viewList viewAlbum albumList.childFirst ]
                        ++ List.map (viewAlbumOrList viewList viewAlbum) albumList.childRest
               )


viewAlbumOrList : (AlbumList -> msg) -> (Album -> msg) -> AlbumOrList -> Html msg
viewAlbumOrList viewList viewAlbum albumOrList =
    let
        childStyles =
            styles [ color white ]
    in
    case albumOrList of
        List albumList ->
            div
                [ childStyles
                , onClick <| viewList albumList
                ]
                [ renderListImage (viewList albumList) albumList.listThumbnail
                , span [] [ Html.Styled.text albumList.listTitle ]
                ]

        Leaf album ->
            div
                [ childStyles
                , onClick <| viewAlbum album
                ]
                [ renderListImage (viewAlbum album) album.thumbnail
                , span [] [ Html.Styled.text album.title ]
                ]


renderListImage : msg -> Image -> Html msg
renderListImage selectedMsg img =
    let
        ( xScaled, yScaled ) =
            if img.srcSetFirst.x > img.srcSetFirst.y then
                ThumbPage.sizeForWidth 200 img
            else
                ThumbPage.sizeForHeight 200 img

        sideMargin =
            10
                + (max
                    0
                   <|
                    toFloat (yScaled - xScaled)
                        / 2
                  )
    in
    renderPresized
        10
        xScaled
        yScaled
        img.srcSetFirst
        img.srcSetRest
        (ThumbPage.thumbStyles
            ++ [ verticalAlign middle
               , Css.marginLeft <| px sideMargin
               , Css.marginRight <| px sideMargin
               ]
        )
        []
    <|
        Just selectedMsg
