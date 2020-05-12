module AlbumListPage exposing (AlbumListPage(..), hashForList, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import ImageViews exposing (..)
import ThumbPage exposing (albumTitle, viewThumb)
import Utils.AlbumUtils exposing (..)


type AlbumListPage
    = AlbumListPage { albumList : AlbumList, bodyViewport : Viewport, parents : List ( AlbumList, Maybe Float ) }


view : AlbumListPage -> (AlbumList -> msg) -> (Album -> msg) -> (Viewport -> msg) -> MainAlbumFlags -> Html msg
view (AlbumListPage alp) viewList viewAlbum scrollMsgMaker flags =
    rootDivFlex
        flags
        column
        (Just scrollMsgMaker)
        alp.bodyViewport
        []
    <|
        [ albumTitle alp.albumList.listTitle (List.map Tuple.first alp.parents) viewList [] [] ]
            ++ (List.reverse <|
                    [ viewAlbumOrList viewList viewAlbum alp.albumList.childFirst ]
                        ++ List.map (viewAlbumOrList viewList viewAlbum) alp.albumList.childRest
               )


viewAlbumOrList : (AlbumList -> msg) -> (Album -> msg) -> AlbumOrList -> Html msg
viewAlbumOrList viewList viewAlbum albumOrList =
    let
        childStyles =
            styles
                [ color white
                , displayFlex
                , flexDirection row
                , alignItems center
                ]
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


hashForList : AlbumListPage -> String
hashForList (AlbumListPage alp) =
    if List.isEmpty alp.parents then
        hashFromAlbumPath [ "" ] []

    else
        hashFromAlbumPath [ alp.albumList.listTitle ] <| List.map Tuple.first alp.parents
