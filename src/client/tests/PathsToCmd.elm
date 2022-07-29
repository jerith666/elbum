module PathsToCmd exposing (suite)

import Album exposing (AlbumOrList(..), Image)
import AlbumListPage exposing (AlbumListPage(..))
import AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..))
import Expect
import Main exposing (AlbumMsg(..), MainAlbumModel(..), MainAlbumMsg(..), PostLoadNavState(..), pathsToCmd)
import Test exposing (Test, describe, test)
import Url exposing (Protocol(..), Url)
import Utils.Loading exposing (ManyModel(..), initMany)


img : Image
img =
    { altText = "img"
    , srcSetFirst = { url = "url", x = 1, y = 2 }
    , srcSetRest = []
    }


url =
    { protocol = Https
    , host = "example.com"
    , port_ = Nothing
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }


model : MainAlbumModel
model =
    LoadedList
        { baseUrl = url
        , flags = { scrollSupport = True }
        , home = Nothing
        , rootDivViewport = Nothing
        , navState = NavInactive
        , listPage = listPage
        }


album =
    { title = "North America"
    , thumbnail = img
    , imageFirst = img
    , imageRest = []
    }


list =
    { listTitle = "World"
    , childFirst = Leaf album
    , childRest = []
    , listThumbnail = img
    }


viewport =
    { scene =
        { width = 1024
        , height = 768
        }
    , viewport =
        { x = 0
        , y = 0
        , width = 102
        , height = 76
        }
    }


listPage =
    AlbumListPage
        { albumList = list
        , bodyViewport = viewport
        , parents = []
        }


suite : Test
suite =
    describe "pathsToCmd"
        [ test "no path produces no command" <|
            \_ ->
                Expect.equal Nothing <|
                    pathsToCmd
                        model
                        Nothing
        , test "empty path produces ViewList for root" <|
            \_ ->
                Expect.equal (Just <| Album <| ViewList listPage Nothing) <|
                    pathsToCmd
                        model
                    <|
                        Just []
        , test "bad 1-level path produces ViewList for root" <|
            \_ ->
                Expect.equal (Just <| Album <| ViewList listPage Nothing) <|
                    pathsToCmd
                        model
                    <|
                        Just [ "bad path" ]
        , test "1-level path produces ViewAlbum for child" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Album <|
                            ViewAlbum
                                (Thumbs
                                    { album = album
                                    , vpInfo = { bodyViewport = viewport, rootDivViewport = Nothing }
                                    , baseUrl = url
                                    , imageLoader =
                                        Tuple.first <| initMany [ { url | path = "/url" } ] [] LoadingMsg
                                    }
                                )
                                [ ( list, Nothing ) ]
                    )
                <|
                    pathsToCmd
                        model
                    <|
                        Just [ "North America" ]
        ]
