module PathsToCmd exposing (suite)

import Album exposing (Album, AlbumList, AlbumOrList(..), Image)
import AlbumListPage exposing (AlbumListPage(..))
import AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..))
import Browser.Dom exposing (Viewport)
import Expect
import Main exposing (AlbumMsg(..), MainAlbumModel(..), MainAlbumMsg(..), MetaMsg(..), PostLoadNavState(..), pathsToCmd)
import Test exposing (Test, describe, test)
import Url exposing (Protocol(..), Url)
import Utils.Loading exposing (ManyModel(..), initMany)



-- some simple pieces needed by albums etc.


img : Image
img =
    { altText = "img"
    , srcSetFirst = { url = "url", x = 1, y = 2 }
    , srcSetRest = []
    }


url : Url
url =
    { protocol = Https
    , host = "example.com"
    , port_ = Nothing
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }


viewport : Viewport
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


{-| makes a basic album with the given title
-}
album : String -> Album
album title =
    { title = title
    , thumbnail = img
    , imageFirst = img
    , imageRest = []
    }


{-| makes a list of albums with the given titles
-}
leaves : String -> String -> List String -> AlbumList
leaves title firstTitle restTitles =
    list title
        (Leaf <| album firstTitle)
        (List.map (Leaf << album) restTitles)


{-| makes an AlbumList from the given children
-}
list : String -> AlbumOrList -> List AlbumOrList -> AlbumList
list title first rest =
    { listTitle = title
    , childFirst = first
    , childRest = rest
    , listThumbnail = img
    }


oneLevelListPage : AlbumListPage
oneLevelListPage =
    AlbumListPage
        { albumList = leaves "World" "North America" []
        , bodyViewport = viewport
        , parents = []
        }


oneLevelModel : MainAlbumModel
oneLevelModel =
    LoadedList
        { baseUrl = url
        , flags = { scrollSupport = True }
        , home = Nothing
        , rootDivViewport = Nothing
        , navState = NavInactive
        , listPage = oneLevelListPage
        }


twoLevelListPage : AlbumListPage
twoLevelListPage =
    AlbumListPage
        { albumList = list "World" (List <| leaves "North America" "Canada" []) []
        , bodyViewport = viewport
        , parents = []
        }


twoLevelModel : MainAlbumModel
twoLevelModel =
    LoadedList
        { baseUrl = url
        , flags = { scrollSupport = True }
        , home = Nothing
        , rootDivViewport = Nothing
        , navState = NavInactive
        , listPage = twoLevelListPage
        }


suite : Test
suite =
    describe "pathsToCmd"
        [ test "no path produces no command" <|
            \_ ->
                Expect.equal Nothing <|
                    pathsToCmd
                        oneLevelModel
                        Nothing
        , test "empty path produces ViewList for root" <|
            \_ ->
                Expect.equal (Just <| Album_ <| ViewList oneLevelListPage Nothing) <|
                    pathsToCmd
                        oneLevelModel
                    <|
                        Just []
        , test "bad 1-level path produces ViewList for root" <|
            \_ ->
                Expect.equal (Just <| Album_ <| ViewList oneLevelListPage Nothing) <|
                    pathsToCmd
                        oneLevelModel
                    <|
                        Just [ "bad path" ]
        , test "1-level path produces ViewAlbum for child" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Album_ <|
                            ViewAlbum
                                (Thumbs
                                    { album = album "North America"
                                    , vpInfo = { bodyViewport = viewport, rootDivViewport = Nothing }
                                    , baseUrl = url
                                    , imageLoader =
                                        Tuple.first <| initMany [ { url | path = "/url" } ] [] LoadingMsg
                                    }
                                )
                                [ ( leaves "World" "North America" [], Nothing ) ]
                    )
                <|
                    pathsToCmd
                        oneLevelModel
                    <|
                        Just [ "North America" ]

        -- 2-level album
        , test "no path produces no command, 2-level" <|
            \_ ->
                Expect.equal Nothing <|
                    pathsToCmd twoLevelModel Nothing
        , test "empty path produces ViewList for root, 2-level" <|
            \_ ->
                Expect.equal (Just <| Album_ <| ViewList twoLevelListPage Nothing) <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just []
        , test "bad 1-level path produces ViewList for root, 2-level" <|
            \_ ->
                Expect.equal (Just <| Album_ <| ViewList twoLevelListPage Nothing) <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just [ "Australia" ]

        {-
           , test "bad 2-level path produces ViewList for root, 2-level" <|
               \_ ->
                   Expect.equal (Just <| Album_ <| ViewList twoLevelListPage Nothing) <|
                       pathsToCmd
                           twoLevelModel
                       <|
                           Just [ "North America", "Mexico" ]
        -}
        , test "1-level path produces ViewAlbum for child, two level" <|
            \_ ->
                Expect.equal
                    (Just
                        (Meta
                            (Sequence
                                (Album_
                                    (ViewList
                                        (AlbumListPage
                                            { albumList =
                                                leaves "North America" "Canada" []
                                            , bodyViewport = viewport
                                            , parents =
                                                [ ( list "World" (List <| leaves "North America" "Canada" []) []
                                                  , Nothing
                                                  )
                                                ]
                                            }
                                        )
                                        Nothing
                                    )
                                )
                                [ Album_ NavCompletedLocally ]
                            )
                        )
                    )
                <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just [ "North America" ]
        ]
