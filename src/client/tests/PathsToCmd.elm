module PathsToCmd exposing (suite)

import Album exposing (AlbumOrList(..), Image)
import AlbumListPage exposing (AlbumListPage(..))
import Expect
import Main exposing (AlbumMsg(..), MainAlbumModel(..), MainAlbumMsg(..), PostLoadNavState(..), pathsToCmd)
import Test exposing (Test, describe, test)
import Url exposing (Protocol(..), Url)


img : Image
img =
    { altText = "img"
    , srcSetFirst = { url = "url", x = 1, y = 2 }
    , srcSetRest = []
    }


model : MainAlbumModel
model =
    LoadedList
        { baseUrl =
            { protocol = Https
            , host = "example.com"
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }
        , flags = { scrollSupport = True }
        , home = Nothing
        , rootDivViewport = Nothing
        , navState = NavInactive
        , listPage = listPage
        }


listPage =
    AlbumListPage
        { albumList =
            { listTitle = "World"
            , childFirst =
                Leaf
                    { title = "North America"
                    , thumbnail = img
                    , imageFirst = img
                    , imageRest = []
                    }
            , childRest = []
            , listThumbnail = img
            }
        , bodyViewport =
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
        ]
