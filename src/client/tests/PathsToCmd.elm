module PathsToCmd exposing (suite)

import Album exposing (Album, AlbumList, AlbumOrList(..), Image)
import AlbumListPage exposing (AlbumListPage(..))
import AlbumPage exposing (AlbumPage(..), AlbumPageMsg(..))
import Browser.Dom exposing (Viewport)
import Expect
import Main exposing (AlbumMsg(..), MainAlbumModel(..), MainAlbumMsg(..), MetaMsg(..), PostLoadNavState(..), pathsToCmd, update)
import Test exposing (Test, describe, test)
import Url exposing (Protocol(..), Url)
import Utils.Loading exposing (initMany)



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


{-| makes an AlbumList from the given children
-}
list : String -> AlbumOrList -> List AlbumOrList -> AlbumList
list listTitle first rest =
    { listTitle = listTitle
    , childFirst = first
    , childRest = rest
    , listThumbnail = img
    }


{-| makes a list of albums with the given titles
-}
leaves : String -> String -> List String -> AlbumList
leaves listTitle firstTitle restTitles =
    list listTitle
        (Leaf <| album firstTitle)
        (List.map (Leaf << album) restTitles)


{-| makes a list page around the given album list
-}
listPage : AlbumList -> AlbumListPage
listPage albumList =
    AlbumListPage
        { albumList = albumList
        , bodyViewport = viewport
        , parents = []
        }


{-| makes a full model around the given list page
-}
model : AlbumListPage -> MainAlbumModel
model listPage_ =
    LoadedList
        { baseUrl = url
        , flags = { scrollSupport = True }
        , home = Nothing
        , rootDivViewport = Nothing
        , navState = NavInactive
        , listPage = listPage_
        }


nList : List String -> String -> String -> AlbumList
nList grandParents parent leaf =
    case grandParents of
        [] ->
            leaves parent leaf []

        greatestGrandParent :: otherGrandParents ->
            list greatestGrandParent (List <| nList otherGrandParents parent leaf) []


oneLevelListPage : AlbumListPage
oneLevelListPage =
    listPage <| nList [] "src" "2004"


oneLevelModel : MainAlbumModel
oneLevelModel =
    model oneLevelListPage


twoLevelListPage : AlbumListPage
twoLevelListPage =
    listPage <| nList [ "src" ] "2004" "Road Trip"


twoLevelModel : MainAlbumModel
twoLevelModel =
    model twoLevelListPage


threeLevelListPage : AlbumListPage
threeLevelListPage =
    listPage <| nList [ "src", "2004" ] "Road Trip" "Highlights"


threeLevelModel : MainAlbumModel
threeLevelModel =
    model threeLevelListPage


fourLevelListPage : AlbumListPage
fourLevelListPage =
    listPage <| nList [ "src", "2004", "Road Trip" ] "Highlights" "UT"


fourLevelModel : MainAlbumModel
fourLevelModel =
    model fourLevelListPage


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
                Expect.equal
                    (Just <|
                        Meta <|
                            Sequence
                                (Album_ <| ViewList oneLevelListPage Nothing)
                                [ Album_ NavCompletedLocally ]
                    )
                <|
                    pathsToCmd
                        oneLevelModel
                    <|
                        Just []
        , test "bad 1-level path produces ViewList for root" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Meta <|
                            Sequence
                                (Album_ <| ViewList oneLevelListPage Nothing)
                                [ Album_ NavCompletedLocally ]
                    )
                <|
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
                                    { album = album "2004"
                                    , vpInfo = { bodyViewport = viewport, rootDivViewport = Nothing }
                                    , baseUrl = url
                                    , imageLoader = Tuple.first <| initMany [ { url | path = "/url" } ] [] LoadingMsg
                                    }
                                )
                                [ ( leaves "src" "2004" [], Nothing ) ]
                    )
                <|
                    pathsToCmd
                        oneLevelModel
                    <|
                        Just [ "2004" ]

        -- 2-level album
        , test "no path produces no command, 2-level" <|
            \_ ->
                Expect.equal
                    Nothing
                <|
                    pathsToCmd twoLevelModel Nothing
        , test "empty path produces ViewList for root, 2-level" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Meta <|
                            Sequence
                                (Album_ <| ViewList twoLevelListPage Nothing)
                                [ Album_ NavCompletedLocally ]
                    )
                <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just []
        , test "bad 1-level path produces ViewList for root, 2-level" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Meta <|
                            Sequence
                                (Album_ <| ViewList twoLevelListPage Nothing)
                                [ Album_ NavCompletedLocally ]
                    )
                <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just [ "Australia" ]
        , test "bad 2-level path produces ViewList for root, 2-level" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Meta <|
                            Sequence
                                (Album_ <| ViewList twoLevelListPage Nothing)
                                [ Album_ NavCompletedLocally ]
                    )
                <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just [ "South America", "Brazil" ]
        , test "partially-valid 2-level path produces ViewAlbum for child" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Album_ <|
                            ViewList
                                (AlbumListPage
                                    { albumList = leaves "2004" "Road Trip" []
                                    , bodyViewport = viewport
                                    , parents = [ ( nList [ "src" ] "2004" "Road Trip", Nothing ) ]
                                    }
                                )
                                Nothing
                     -- TODO NavCompletedLocally is missing!
                    )
                <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just [ "2004", "Mexico" ]
        , test "1-level path produces ViewAlbum for child, two level" <|
            \_ ->
                Expect.equal
                    (Just <|
                        Meta <|
                            Sequence
                                (Album_ <|
                                    ViewList
                                        (AlbumListPage
                                            { albumList = leaves "2004" "Road Trip" []
                                            , bodyViewport = viewport
                                            , parents = [ ( nList [ "src" ] "2004" "Road Trip", Nothing ) ]
                                            }
                                        )
                                        Nothing
                                )
                                [ Album_ NavCompletedLocally ]
                    )
                <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just [ "2004" ]
        , test "2-level path produces ViewAlbum for grand-child" <|
            \_ ->
                Expect.equal
                    (Just
                        (Album_
                            (ViewAlbum
                                (Thumbs
                                    { album = album "Road Trip"
                                    , vpInfo = { bodyViewport = viewport, rootDivViewport = Nothing }
                                    , baseUrl = url
                                    , imageLoader = Tuple.first <| initMany [ { url | path = "/url" } ] [] LoadingMsg
                                    }
                                )
                                [ ( nList [] "2004" "Road Trip", Nothing )
                                , ( nList [ "src" ] "2004" "Road Trip", Nothing )
                                ]
                            )
                        )
                    )
                <|
                    pathsToCmd
                        twoLevelModel
                    <|
                        Just [ "2004", "Road Trip" ]
        , test "2-level path then back produces ViewAlbum for child" <|
            \_ ->
                let
                    msgFor2LevelPath =
                        pathsToCmd
                            twoLevelModel
                        <|
                            Just [ "2004", "Road Trip" ]

                    mUpdate msg =
                        Tuple.first <| update msg twoLevelModel

                    modelAfter2LevelPath =
                        Maybe.map mUpdate msgFor2LevelPath

                    msgAfter1LevelPath =
                        Maybe.andThen (\model_ -> pathsToCmd model_ <| Just [ "2004" ]) modelAfter2LevelPath
                in
                Expect.equal
                    (Just
                        (Album_
                            (ViewList
                                (AlbumListPage
                                    { albumList = leaves "2004" "Road Trip" []
                                    , bodyViewport = viewport
                                    , parents = [ ( nList [ "src" ] "2004" "Road Trip", Nothing ) ]
                                    }
                                )
                                Nothing
                            )
                        )
                    )
                    msgAfter1LevelPath
        , test "2-level path then back produces ViewAlbum for child, 3 level model" <|
            \_ ->
                let
                    msgFor2LevelPath =
                        pathsToCmd
                            threeLevelModel
                        <|
                            Just [ "2004", "Road Trip" ]

                    mUpdate msg =
                        Tuple.first <| update msg threeLevelModel

                    modelAfter2LevelPath =
                        Maybe.map mUpdate msgFor2LevelPath

                    msgAfter1LevelPath =
                        Maybe.andThen (\model_ -> pathsToCmd model_ <| Just [ "2004" ]) modelAfter2LevelPath
                in
                Expect.equal
                    (Just <|
                        Meta <|
                            Sequence
                                (Album_
                                    (ViewList
                                        (AlbumListPage
                                            { albumList = nList [ "2004" ] "Road Trip" "Highlights"
                                            , bodyViewport = viewport
                                            , parents =
                                                [ ( nList [] "Road Trip" "Highlights", Nothing )
                                                , ( nList [ "2004" ] "Road Trip" "Highlights", Nothing )
                                                , ( nList [ "src", "2004" ] "Road Trip" "Highlights", Nothing )
                                                ]
                                            }
                                        )
                                        Nothing
                                    )
                                )
                                [ Album_ NavCompletedLocally ]
                    )
                    msgAfter1LevelPath
        , test "4-level path then back produces ViewAlbum for child, 4 level model" <|
            \_ ->
                let
                    msgFor4LevelPath =
                        pathsToCmd
                            fourLevelModel
                        <|
                            Just [ "2004", "Road Trip", "Highlights", "UT" ]

                    mUpdate msg =
                        Tuple.first <| update msg fourLevelModel

                    modelAfter4LevelPath =
                        Maybe.map mUpdate msgFor4LevelPath

                    msgAfter3LevelPath =
                        Maybe.andThen (\model_ -> pathsToCmd model_ <| Just [ "2004", "Road Trip", "Highlights" ]) modelAfter4LevelPath
                in
                Expect.equal
                    (Just <|
                                (Album_
                                    (ViewList
                                        (AlbumListPage
                                            { albumList = nList [] "Highlights" "UT"
                                            , bodyViewport = viewport
                                            , parents =
                                                [ ( nList [ "Road Trip" ] "Highlights" "UT", Nothing )
                                                , ( nList [ "2004", "Road Trip" ] "Highlights" "UT", Nothing )
                                                , ( nList [ "src", "2004", "Road Trip" ] "Highlights" "UT", Nothing )
                                                ]
                                            }
                                        )
                                        Nothing
                                    )
                                )
                    )
                    msgAfter3LevelPath
        ]


actual =
    Just
        (Meta
            (Sequence
                (Album_
                    (ViewList
                        (AlbumListPage
                            { albumList =
                                { childFirst =
                                    List
                                        { childFirst =
                                            Leaf
                                                { imageFirst =
                                                    { altText = "img"
                                                    , srcSetFirst =
                                                        { url = "url", x = 1, y = 2 }
                                                    , srcSetRest = []
                                                    }
                                                , imageRest = []
                                                , thumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                , title = "Highlights"
                                                }
                                        , childRest = []
                                        , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                        , listTitle = "Road Trip"
                                        }
                                , childRest = []
                                , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                , listTitle = "2004"
                                }
                            , bodyViewport = { scene = { height = 768, width = 1024 }, viewport = { height = 76, width = 102, x = 0, y = 0 } }
                            , parents =
                                [ ( { childFirst =
                                        Leaf
                                            { imageFirst =
                                                { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                            , imageRest = []
                                            , thumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                            , title = "Highlights"
                                            }
                                    , childRest = []
                                    , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                    , listTitle = "Road Trip"
                                    }
                                  , Nothing
                                  )
                                , ( { childFirst =
                                        List
                                            { childFirst =
                                                Leaf
                                                    { imageFirst =
                                                        { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                    , imageRest = []
                                                    , thumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                    , title = "Highlights"
                                                    }
                                            , childRest = []
                                            , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                            , listTitle = "Road Trip"
                                            }
                                    , childRest = []
                                    , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                    , listTitle = "2004"
                                    }
                                  , Nothing
                                  )
                                , ( { childFirst =
                                        List
                                            { childFirst =
                                                List
                                                    { childFirst =
                                                        Leaf
                                                            { imageFirst =
                                                                { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                            , imageRest = []
                                                            , thumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                            , title = "Highlights"
                                                            }
                                                    , childRest = []
                                                    , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                    , listTitle = "Road Trip"
                                                    }
                                            , childRest = []
                                            , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                            , listTitle = "2004"
                                            }
                                    , childRest = []
                                    , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                    , listTitle = "src"
                                    }
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


expected =
    Just
        (Meta
            (Sequence
                (Album_
                    (ViewList
                        (AlbumListPage
                            { albumList =
                                { childFirst =
                                    List
                                        { childFirst =
                                            Leaf
                                                { imageFirst =
                                                    { altText = "img"
                                                    , srcSetFirst =
                                                        { url = "url", x = 1, y = 2 }
                                                    , srcSetRest = []
                                                    }
                                                , imageRest = []
                                                , thumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                , title = "Highlights"
                                                }
                                        , childRest = []
                                        , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                        , listTitle = "Road Trip"
                                        }
                                , childRest = []
                                , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                , listTitle = "2004"
                                }
                            , bodyViewport = { scene = { height = 768, width = 1024 }, viewport = { height = 76, width = 102, x = 0, y = 0 } }
                            , parents =
                                [ ( { childFirst =
                                        List
                                            { childFirst =
                                                List
                                                    { childFirst =
                                                        Leaf
                                                            { imageFirst =
                                                                { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                            , imageRest = []
                                                            , thumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                            , title = "Highlights"
                                                            }
                                                    , childRest = []
                                                    , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                                    , listTitle = "Road Trip"
                                                    }
                                            , childRest = []
                                            , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                            , listTitle = "2004"
                                            }
                                    , childRest = []
                                    , listThumbnail = { altText = "img", srcSetFirst = { url = "url", x = 1, y = 2 }, srcSetRest = [] }
                                    , listTitle = "src"
                                    }
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
