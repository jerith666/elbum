module AlbumTreeNodePage exposing (AlbumTreeNodePage(..), AlbumTreeNodePageMsg, view)

import Html exposing (..)
import Album exposing (..)
import WinSize exposing (..)


type AlbumTreeNodePage
    = AlbumTreeNodePage AlbumTreeNode WinSize (Maybe AlbumTreeNode)


type AlbumTreeNodePageMsg
    = String


view (AlbumTreeNodePage albumTreeNode winSize parent) =
    Html.text "hello world"
