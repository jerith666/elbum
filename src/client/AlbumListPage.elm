module AlbumListPage exposing (AlbumListPage(..), view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import ThumbPage exposing (albumTitle, viewThumb)
import WinSize exposing (..)


type AlbumListPage
    = AlbumListPage AlbumList WinSize (List AlbumList)


view : AlbumListPage -> (AlbumList -> msg) -> (Album -> msg) -> AlbumBootstrapFlags -> Html msg
view (AlbumListPage albumList winSize parents) viewList viewAlbum flags =
    rootDivFlex
        flags
        column
        []
    <|
        [ albumTitle albumList.listTitle parents viewList [] ]
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
                [ viewThumb 200 Completed [ verticalAlign middle ] (viewList albumList) albumList.listThumbnail
                , span [] [ Html.Styled.text albumList.listTitle ]
                ]

        Leaf album ->
            div
                [ childStyles
                , onClick <| viewAlbum album
                ]
                [ viewThumb 200 Completed [ verticalAlign middle ] (viewAlbum album) album.thumbnail
                , span [] [ Html.Styled.text album.title ]
                ]
