module AlbumTreeNodePage exposing (AlbumTreeNodePage(..), view)

import Html exposing (..)
import Html.Events exposing (..)
import Css exposing (..)
import Album exposing (..)
import AlbumStyles exposing (..)
import WinSize exposing (..)


type AlbumTreeNodePage
    = AlbumTreeNodePage AlbumTreeNode WinSize (List AlbumTreeNode)


view (AlbumTreeNodePage albumTreeNode winSize parent) viewSubtree viewAlbum =
    rootDivFlex
        column
        []
    <|
        [ viewTitle albumTreeNode.nodeTitle ]
            ++ [ viewChildNode viewSubtree viewAlbum albumTreeNode.childFirst ]
            ++ List.map (viewChildNode viewSubtree viewAlbum) albumTreeNode.childRest


viewTitle title =
    div
        [ styles
            [ color white
            , textAlign center
            , Css.width (vw 100)
            , padding (px 5)
            ]
        ]
        [ Html.text <| "album tree node page for " ++ title ]


viewChildNode viewSubtree viewAlbum nodeOrAlbum =
    case nodeOrAlbum of
        Subtree albumTreeNode ->
            div
                [ styles [ color white ]
                , onClick <| viewSubtree albumTreeNode
                ]
                [ Html.text <| "link to sub album " ++ albumTreeNode.nodeTitle ]

        Leaf album ->
            div
                [ styles [ color white ]
                , onClick <| viewAlbum album
                ]
                [ Html.text <| "link to leaf album " ++ album.title ]
