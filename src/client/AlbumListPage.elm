module AlbumListPage exposing (AlbumListPage(..), hashForList, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import ImageViews exposing (..)
import ThumbPage exposing (albumTitle)
import Utils.AlbumUtils exposing (..)


type AlbumListPage
    = AlbumListPage { albumList : AlbumList, bodyViewport : Viewport, parents : List ( AlbumList, Maybe Float ) }


view : AlbumListPage -> (msg -> List (Attribute msg) -> List (Html msg) -> Html msg) -> (AlbumList -> msg) -> (Album -> msg) -> (Viewport -> msg) -> MainAlbumFlags -> Html msg
view (AlbumListPage alp) a viewList viewAlbum scrollMsgMaker flags =
    rootDivFlex
        flags
        column
        (Just scrollMsgMaker)
        alp.bodyViewport
        []
    <|
        [ albumTitle alp.albumList.listTitle (List.map Tuple.first alp.parents) viewList [] [ position fixed ]
        , albumTitle alp.albumList.listTitle (List.map Tuple.first alp.parents) viewList [] [ visibility hidden ]
        ]
            ++ (List.reverse <|
                    [ viewAlbumOrList a viewList viewAlbum alp.albumList.childFirst ]
                        ++ List.map (viewAlbumOrList a viewList viewAlbum) alp.albumList.childRest
               )


viewAlbumOrList : (msg -> List (Attribute msg) -> List (Html msg) -> Html msg) -> (AlbumList -> msg) -> (Album -> msg) -> AlbumOrList -> Html msg
viewAlbumOrList a viewList viewAlbum albumOrList =
    let
        childStyles =
            styles
                [ color white
                , displayFlex
                , flexDirection row
                , alignItems center
                , flexShrink <| int 0
                ]
    in
    case albumOrList of
        List albumList ->
            a (viewList albumList)
                [ styles [ textDecoration none ] ]
                [ div
                    [ childStyles ]
                    [ renderListImage albumList.listThumbnail
                    , span [ styles [ flexShrink <| int 1 ] ] [ Html.Styled.text albumList.listTitle ]
                    ]
                ]

        Leaf album ->
            a (viewAlbum album)
                [ styles [ textDecoration none ] ]
                [ div
                    [ childStyles ]
                    [ renderListImage album.thumbnail
                    , span [ styles [ flexShrink <| int 1 ] ] [ Html.Styled.text album.title ]
                    ]
                ]


renderListImage : Image -> Html msg
renderListImage img =
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
               , flexShrink <| int 0
               ]
        )
        []


hashForList : AlbumListPage -> String
hashForList (AlbumListPage alp) =
    if List.isEmpty alp.parents then
        hashFromAlbumPath [ "" ] []

    else
        hashFromAlbumPath [ alp.albumList.listTitle ] <| List.map Tuple.first alp.parents
