module AlbumTreeNodePage exposing (AlbumTreeNodePage(..), AlbumTreeNodePageMsg, view)

import Html exposing (..)
import Css exposing (..)
import Album exposing (..)
import AlbumStyles exposing (..)
import WinSize exposing (..)


type AlbumTreeNodePage
    = AlbumTreeNodePage AlbumTreeNode WinSize (List AlbumTreeNode)


type AlbumTreeNodePageMsg
    = String


view (AlbumTreeNodePage albumTreeNode winSize parent) =
    rootDivFlex
        column
        []
    <|
        [ viewTitle albumTreeNode.nodeTitle ]
            ++ [ viewChildNode albumTreeNode.childFirst ]
            ++ List.map viewChildNode albumTreeNode.childRest


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


viewChildNode nodeOrAlbum =
    case nodeOrAlbum of
        Subtree albumTreeNode ->
            div
                [ styles [ color white ] ]
                [ Html.text <| "link to sub album " ++ albumTreeNode.nodeTitle ]

        Leaf album ->
            div
                [ styles [ color white ] ]
                [ Html.text <| "link to leaf album " ++ album.title ]
