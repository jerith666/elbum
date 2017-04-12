module AlbumTreeNodePage exposing (AlbumTreeNodePage(..), view)

import Html exposing (..)
import Html.Events exposing (..)
import Css exposing (..)
import Album exposing (..)
import ThumbPage exposing (viewThumb)
import AlbumStyles exposing (..)
import WinSize exposing (..)


type AlbumTreeNodePage
    = AlbumTreeNodePage AlbumTreeNode WinSize (List AlbumTreeNode)


view : AlbumTreeNodePage -> (AlbumTreeNode -> msg) -> (Album -> msg) -> Html msg
view (AlbumTreeNodePage albumTreeNode winSize parent) viewSubtree viewAlbum =
    rootDivFlex
        column
        []
    <|
        [ viewTitle albumTreeNode.nodeTitle ]
            ++ [ viewChildNode viewSubtree viewAlbum albumTreeNode.childFirst ]
            ++ List.map (viewChildNode viewSubtree viewAlbum) albumTreeNode.childRest


viewTitle : String -> Html msg
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


thumbFor : NodeOrAlbum -> Image
thumbFor nodeOrAlbum =
    case nodeOrAlbum of
        Subtree albumTreeNode ->
            thumbFor albumTreeNode.childFirst

        Leaf album ->
            album.imageFirst


viewChildNode : (AlbumTreeNode -> msg) -> (Album -> msg) -> NodeOrAlbum -> Html msg
viewChildNode viewSubtree viewAlbum nodeOrAlbum =
    case nodeOrAlbum of
        Subtree albumTreeNode ->
            div
                [ styles [ color white ]
                , onClick <| viewSubtree albumTreeNode
                ]
                [ viewThumb 200 (viewSubtree albumTreeNode) (thumbFor <| Subtree albumTreeNode)
                , div [] [ Html.text <| "link to sub album " ++ albumTreeNode.nodeTitle ]
                ]

        Leaf album ->
            div
                [ styles [ color white ]
                , onClick <| viewAlbum album
                ]
                [ viewThumb 200 (viewAlbum album) album.imageFirst
                , div [] [ Html.text <| "link to leaf album " ++ album.title ]
                ]
