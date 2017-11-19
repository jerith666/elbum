module AlbumTreeNodePage exposing (AlbumTreeNodePage(..), view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import ThumbPage exposing (albumTitle, viewThumb)
import WinSize exposing (..)


type AlbumTreeNodePage
    = AlbumTreeNodePage AlbumTreeNode WinSize (List AlbumTreeNode)


view : AlbumTreeNodePage -> (AlbumTreeNode -> msg) -> (Album -> msg) -> Html msg
view (AlbumTreeNodePage albumTreeNode winSize parents) viewSubtree viewAlbum =
    rootDivFlex
        column
        []
    <|
        [ albumTitle albumTreeNode.nodeTitle parents viewSubtree [] ]
            ++ (List.reverse <|
                    [ viewChildNode viewSubtree viewAlbum albumTreeNode.childFirst ]
                        ++ List.map (viewChildNode viewSubtree viewAlbum) albumTreeNode.childRest
               )


viewChildNode : (AlbumTreeNode -> msg) -> (Album -> msg) -> NodeOrAlbum -> Html msg
viewChildNode viewSubtree viewAlbum nodeOrAlbum =
    case nodeOrAlbum of
        Subtree albumTreeNode ->
            div
                [ styles [ color white ]
                , onClick <| viewSubtree albumTreeNode
                ]
                [ viewThumb 200 ( 1, False ) (viewSubtree albumTreeNode) albumTreeNode.nodeThumbnail
                , div [] [ Html.text albumTreeNode.nodeTitle ]
                ]

        Leaf album ->
            div
                [ styles [ color white ]
                , onClick <| viewAlbum album
                ]
                [ viewThumb 200 ( 1, False ) (viewAlbum album) album.thumbnail
                , div [] [ Html.text album.title ]
                ]
