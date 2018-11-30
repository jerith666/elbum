module Main exposing (main)

import Album exposing (..)
import AlbumListPage exposing (..)
import AlbumPage exposing (..)
import AlbumStyles exposing (..)
import Browser exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Browser.Navigation exposing (..)
import Css exposing (..)
import Debouncer.Messages exposing (..)
import DebugSupport exposing (debugString, log)
import Delay exposing (..)
import Dict exposing (..)
import FullImagePage exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (..)
import KeyboardUtils exposing (onEscape)
import ListUtils exposing (..)
import LocationUtils exposing (..)
import ResultUtils exposing (..)
import RouteUrl exposing (..)
import Set exposing (..)
import Task exposing (..)
import Time exposing (..)
import Url exposing (..)


type AlbumBootstrap
    = Sizing
        { key : Key
        , flags : AlbumBootstrapFlags
        , paths : Maybe (List String)
        , scroll : Maybe Float
        }
    | LoadingHomeLink
        { key : Key
        , bodyViewport : Viewport
        , flags : AlbumBootstrapFlags
        , paths : Maybe (List String)
        , scroll : Maybe Float
        }
    | Loading
        { key : Key
        , bodyViewport : Viewport
        , progress : Maybe Progress
        , flags : AlbumBootstrapFlags
        , home : Maybe String
        , paths : Maybe (List String)
        , scroll : Maybe Float
        }
    | LoadError
        { key : Key
        , flags : AlbumBootstrapFlags
        , error : Http.Error
        }
    | LoadedList
        { key : Key
        , listPage : AlbumListPage
        , flags : AlbumBootstrapFlags
        , home : Maybe String
        , pendingUrls : Dict String UrlLoadState
        , rootDivViewport : Maybe Viewport
        , navState : PostLoadNavState
        , debouncer : Debouncer AlbumBootstrapMsg
        }
    | LoadedAlbum
        { key : Key
        , albumPage : AlbumPage
        , parents : List ( AlbumList, Maybe Float )
        , flags : AlbumBootstrapFlags
        , home : Maybe String
        , pendingUrls : Dict String UrlLoadState
        , rootDivViewport : Maybe Viewport
        , navState : PostLoadNavState
        , debouncer : Debouncer AlbumBootstrapMsg
        }


type PostLoadNavState
    = NavInProgress
    | NavInactive


type UrlLoadState
    = UrlRequested
      --| Partial Int
    | JustCompleted
    | ReadyToDisplay
    | Failed Http.Error


type AlbumBootstrapMsg
    = Resize Viewport
    | YesHome String
    | NoHome Http.Error
    | LoadAlbumProgress Progress
    | YesAlbum AlbumOrList
    | NoAlbum Http.Error
    | PageMsg AlbumPage.AlbumPageMsg
    | ViewList AlbumListPage (Maybe Float)
    | ViewAlbum AlbumPage (List ( AlbumList, Maybe Float ))
    | ImageLoaded String
    | ImageReadyToDisplay String
    | ImageFailed String Http.Error
    | ScheduleScroll (Maybe Float)
    | ScrolledTo Viewport
    | DebouncerMsg (Debouncer.Messages.Msg AlbumBootstrapMsg)
    | ScrollSucceeded
    | ScrollFailed String
    | Nav (List String)
    | Scroll Float
    | Sequence AlbumBootstrapMsg (List AlbumBootstrapMsg)
    | SequenceCmd (Cmd AlbumBootstrapMsg) (List (Cmd AlbumBootstrapMsg))
    | LinkClicked UrlRequest
    | NoBootstrap


albumJson : String
albumJson =
    "album.json"


main : RouteUrlProgram AlbumBootstrapFlags AlbumBootstrap AlbumBootstrapMsg
main =
    RouteUrl.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , delta2url = locFor
        , location2messages = navToMsg
        }


init : AlbumBootstrapFlags -> Key -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
init flags key =
    ( Sizing { key = key, flags = flags, paths = Nothing, scroll = Nothing }
    , Task.perform Resize getViewport
    )


update : AlbumBootstrapMsg -> AlbumBootstrap -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
update msg model =
    log "update produces new (model,msg)" <|
        case log "update msg" msg of
            Resize viewport ->
                case model of
                    Sizing sz ->
                        ( LoadingHomeLink { key = sz.key, bodyViewport = log "window size set" viewport, flags = sz.flags, paths = sz.paths, scroll = sz.scroll }
                        , Http.get { url = "home", expect = expectString <| either NoHome YesHome }
                        )

                    LoadingHomeLink lh ->
                        ( LoadingHomeLink { lh | bodyViewport = viewport }
                        , Cmd.none
                        )

                    Loading ld ->
                        ( Loading { ld | bodyViewport = log "window size updated during load" viewport }
                        , Cmd.none
                        )

                    LoadError _ ->
                        ( model, Cmd.none )

                    LoadedAlbum la ->
                        case la.albumPage of
                            Thumbs th ->
                                let
                                    oldVpInfo =
                                        th.vpInfo

                                    newModel =
                                        Thumbs { th | vpInfo = { oldVpInfo | bodyViewport = log "window size updated for thumbs" viewport } }

                                    urls =
                                        AlbumPage.urlsToGet newModel
                                in
                                ( LoadedAlbum
                                    { la
                                        | albumPage = newModel
                                        , pendingUrls =
                                            Dict.union la.pendingUrls <|
                                                dictWithValues urls UrlRequested
                                    }
                                , getUrls la.pendingUrls urls
                                )

                            FullImage fi ->
                                let
                                    oldVpInfo =
                                        fi.vpInfo
                                in
                                ( LoadedAlbum { la | albumPage = FullImage { fi | vpInfo = { oldVpInfo | bodyViewport = log "window size updated for full" viewport } } }
                                , Cmd.none
                                )

                    LoadedList ll ->
                        case ll.listPage of
                            AlbumListPage alp ->
                                ( LoadedList { ll | listPage = AlbumListPage { alp | bodyViewport = viewport } }
                                , Cmd.none
                                )

            YesHome home ->
                case model of
                    LoadingHomeLink lh ->
                        gotHome lh <| Just <| String.trim home

                    _ ->
                        ( model, Cmd.none )

            NoHome err ->
                case model of
                    LoadingHomeLink lh ->
                        gotHome lh Nothing

                    _ ->
                        ( model, Cmd.none )

            LoadAlbumProgress progress ->
                case model of
                    Loading ld ->
                        ( Loading { ld | progress = Just progress }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            YesAlbum albumOrList ->
                case model of
                    Loading ld ->
                        case albumOrList of
                            List albumList ->
                                let
                                    newModel =
                                        LoadedList
                                            { key = ld.key
                                            , listPage = AlbumListPage { albumList = albumList, bodyViewport = ld.bodyViewport, parents = [] }
                                            , flags = ld.flags
                                            , home = ld.home
                                            , pendingUrls = Dict.empty
                                            , rootDivViewport = Nothing
                                            , navState = NavInactive
                                            , debouncer = debouncer
                                            }

                                    pathsThenScroll =
                                        toCmd <| sequence (pathsToCmd newModel ld.paths) <| fromMaybe <| scrollToCmd newModel ld.scroll
                                in
                                ( newModel
                                , pathsThenScroll
                                )

                            Leaf album ->
                                let
                                    albumPage =
                                        Thumbs
                                            { album = album
                                            , vpInfo = { bodyViewport = ld.bodyViewport, rootDivViewport = Nothing }
                                            , justLoadedImages = Set.empty
                                            , readyToDisplayImages = Set.empty
                                            }

                                    urls =
                                        AlbumPage.urlsToGet albumPage

                                    newModel =
                                        LoadedAlbum
                                            { key = ld.key
                                            , albumPage = albumPage
                                            , parents = []
                                            , flags = ld.flags
                                            , home = ld.home
                                            , pendingUrls = dictWithValues urls UrlRequested
                                            , rootDivViewport = Nothing
                                            , navState = NavInactive
                                            , debouncer = debouncer
                                            }

                                    pathsThenScroll =
                                        toCmd <| sequence (pathsToCmd newModel ld.paths) <| fromMaybe <| scrollToCmd newModel ld.scroll
                                in
                                ( newModel
                                , Cmd.batch
                                    [ getUrls Dict.empty urls
                                    , pathsThenScroll
                                    ]
                                )

                    _ ->
                        ( model, Cmd.none )

            NoAlbum err ->
                ( LoadError { key = keyOf model, flags = flagsOf model, error = err }
                , Cmd.none
                )

            PageMsg pageMsg ->
                case model of
                    LoadedAlbum la ->
                        let
                            ( newPage, newPageCmd ) =
                                AlbumPage.update pageMsg la.albumPage <| Maybe.map scrollPosOf la.rootDivViewport

                            newPendingUrls =
                                if AlbumPage.resetUrls pageMsg then
                                    Dict.empty

                                else
                                    la.pendingUrls

                            urls =
                                AlbumPage.urlsToGet newPage
                        in
                        ( LoadedAlbum { la | albumPage = newPage, pendingUrls = Dict.union newPendingUrls <| dictWithValues urls UrlRequested }
                        , Cmd.batch
                            [ getUrls newPendingUrls urls
                            , Cmd.map PageMsg newPageCmd
                            ]
                        )

                    _ ->
                        ( model, Cmd.none )

            ImageLoaded url ->
                updateImageResult model url JustCompleted

            ImageReadyToDisplay url ->
                updateImageResult model url ReadyToDisplay

            ImageFailed url err ->
                updateImageResult model url <| Failed err

            ViewList albumListPage maybeScroll ->
                let
                    newModel =
                        LoadedList
                            { key = keyOf model
                            , listPage = albumListPage
                            , flags = flagsOf model
                            , home = homeOf model
                            , pendingUrls = Dict.empty
                            , rootDivViewport = Nothing
                            , navState = NavInactive
                            , debouncer = debouncerOf model
                            }

                    scrollCmd =
                        case maybeScroll of
                            Just pos ->
                                Task.attempt (\_ -> NoBootstrap) <| setViewportOf rootDivId 0 pos

                            Nothing ->
                                scrollToTop

                    title =
                        case albumListPage of
                            AlbumListPage alp ->
                                alp.albumList.listTitle
                in
                ( newModel
                , scrollCmd
                )

            ViewAlbum albumPage parents ->
                let
                    urls =
                        AlbumPage.urlsToGet albumPage

                    newModel =
                        LoadedAlbum
                            { key = keyOf model
                            , albumPage = albumPage
                            , parents = parents
                            , flags = flagsOf model
                            , home = homeOf model
                            , pendingUrls = dictWithValues urls UrlRequested
                            , rootDivViewport = Nothing
                            , navState = NavInactive
                            , debouncer = debouncerOf model
                            }
                in
                ( newModel
                , Cmd.batch
                    [ scrollToTop
                    , getUrls Dict.empty urls
                    ]
                )

            ScrolledTo viewport ->
                ( withScrollPos (log "ScrolledTo: " viewport) model, Cmd.none )

            DebouncerMsg subMsg ->
                log "debouncer update produces new (model,msg)" <|
                    Debouncer.Messages.update update
                        { mapMsg = DebouncerMsg
                        , getDebouncer = debouncerOf
                        , setDebouncer =
                            \newDebouncer aModel ->
                                case aModel of
                                    LoadedList ll ->
                                        LoadedList { ll | debouncer = newDebouncer }

                                    LoadedAlbum la ->
                                        LoadedAlbum { la | debouncer = newDebouncer }

                                    _ ->
                                        log "setDebouncer on model type that does not contain one" aModel
                        }
                        (log "debouncer message wrapping" subMsg)
                        model

            ScheduleScroll scroll ->
                ( model
                , Maybe.withDefault Cmd.none <|
                    Maybe.map
                        (\s ->
                            Task.attempt
                                (\_ -> NoBootstrap)
                            <|
                                setViewportOf rootDivId 0 <|
                                    log "startup scroll to" s
                        )
                        scroll
                )

            ScrollSucceeded ->
                ( model, Cmd.none )

            ScrollFailed _ ->
                ( model, Cmd.none )

            Scroll s ->
                ( withScroll model s, toCmd <| Maybe.withDefault NoBootstrap <| scrollToCmd model <| Just s )

            Nav paths ->
                ( withPaths model paths, toCmd <| Maybe.withDefault NoBootstrap <| pathsToCmd model <| Just paths )

            Sequence next rest ->
                let
                    ( nextModel, nextCmd ) =
                        update next model
                in
                case rest of
                    [] ->
                        ( nextModel, log ("sequence msg " ++ debugString next ++ " (last) produces cmd") nextCmd )

                    r1 :: rs ->
                        let
                            ( r1Model, r1Cmds ) =
                                update r1 nextModel

                            ( rModel, rCmds ) =
                                case rs of
                                    [] ->
                                        ( r1Model, r1Cmds )

                                    rs1 :: rss ->
                                        let
                                            ( rsModel, rsCmds ) =
                                                update (Sequence rs1 rss) r1Model
                                        in
                                        ( rsModel, toCmd <| SequenceCmd r1Cmds [ rsCmds ] )
                        in
                        ( rModel, toCmd <| SequenceCmd (log ("sequence msg " ++ debugString next ++ " (cont'd) produces cmd") nextCmd) [ rCmds ] )

            SequenceCmd next rest ->
                let
                    cmds =
                        case rest of
                            [] ->
                                log "sequenced cmd: last" next

                            r1 :: rs ->
                                Cmd.batch [ log "sequenced cmd: next" next, toCmd <| SequenceCmd r1 rs ]
                in
                ( model, cmds )

            LinkClicked urlRequest ->
                case urlRequest of
                    Internal url ->
                        --home link might count as internal if it's on the same domain
                        let
                            hUrl =
                                Maybe.andThen Url.fromString <| homeOf model
                        in
                        case Maybe.withDefault False <| Maybe.map (\h -> h == url) <| hUrl of
                            True ->
                                ( model, load <| toString url )

                            False ->
                                ( log ("ignoring unexpected internal url request not for home url (" ++ (Maybe.withDefault "home not set" <| homeOf model) ++ ") " ++ toString url) model, Cmd.none )

                    External url ->
                        --home link should be only external link in our app
                        ( model, load url )

            NoBootstrap ->
                ( model, Cmd.none )


{-| allow at most 100 scroll updates in 30 seconds
<https://bugs.webkit.org/show_bug.cgi?id=156115>
f = 100/30s
T = 30s/100
T = 0.3s
T = 300ms
-}
debouncer : Debouncer AlbumBootstrapMsg
debouncer =
    --toDebouncer <| throttle (fromSeconds <| 30 / 100)
    --toDebouncer <| throttle (fromSeconds 1)
    toDebouncer <| throttle <| fromSeconds 0.3


throttledScrolledTo : Viewport -> AlbumBootstrapMsg
throttledScrolledTo =
    ScrolledTo >> provideInput >> DebouncerMsg


debouncerOf model =
    case model of
        LoadedList ll ->
            ll.debouncer

        LoadedAlbum la ->
            la.debouncer

        _ ->
            log ("returning default debouncer for model that does not contain one " ++ debugString model) debouncer


sequence : Maybe AlbumBootstrapMsg -> List AlbumBootstrapMsg -> AlbumBootstrapMsg
sequence mm1 ms =
    case mm1 of
        Nothing ->
            case List.tail ms of
                Nothing ->
                    case List.head ms of
                        Nothing ->
                            NoBootstrap

                        Just ms1 ->
                            ms1

                Just mst ->
                    sequence (List.head ms) mst

        Just m1 ->
            Sequence m1 ms


gotHome lh home =
    ( Loading { key = lh.key, bodyViewport = lh.bodyViewport, progress = Nothing, flags = lh.flags, home = home, paths = lh.paths, scroll = lh.scroll }
    , Http.request
        { method = "GET"
        , headers = []
        , url = albumJson
        , body = emptyBody
        , expect = expectJson (either NoAlbum YesAlbum) jsonDecAlbumOrList
        , timeout = Nothing
        , tracker = Just albumJson
        }
    )


navToMsg : Url -> List AlbumBootstrapMsg
navToMsg loc =
    let
        parsedHash =
            log ("parsedHash from " ++ Maybe.withDefault "<no fragment>" loc.fragment) <| parseHash <| Maybe.withDefault "" loc.fragment

        parsedQuery =
            log ("parsedQuery from " ++ Maybe.withDefault "<no query>" loc.query) <| parseQuery <| Maybe.withDefault "" loc.query

        hashMsgs =
            case parsedHash of
                Err _ ->
                    []

                Ok paths ->
                    [ Nav paths ]

        queryMsgs =
            case parsedQuery of
                Err _ ->
                    []

                Ok scroll ->
                    NoBootstrap
                        -- hack delay
                        :: (fromMaybe <|
                                Maybe.map Scroll (scroll |> Maybe.andThen String.toFloat)
                           )
    in
    case hashMsgs ++ queryMsgs of
        [] ->
            []

        [ c ] ->
            [ c ]

        c1 :: cs ->
            [ Sequence c1 cs ]


flagsOf : AlbumBootstrap -> AlbumBootstrapFlags
flagsOf model =
    case model of
        Sizing sz ->
            sz.flags

        LoadingHomeLink lh ->
            lh.flags

        Loading ld ->
            ld.flags

        LoadError le ->
            le.flags

        LoadedList ll ->
            ll.flags

        LoadedAlbum la ->
            la.flags


homeOf : AlbumBootstrap -> Maybe String
homeOf model =
    case model of
        Sizing _ ->
            Nothing

        LoadingHomeLink _ ->
            Nothing

        Loading ld ->
            ld.home

        LoadError _ ->
            Nothing

        LoadedList ll ->
            ll.home

        LoadedAlbum la ->
            la.home


keyOf : AlbumBootstrap -> Key
keyOf model =
    case model of
        Sizing sz ->
            sz.key

        LoadingHomeLink lh ->
            lh.key

        Loading ld ->
            ld.key

        LoadError le ->
            le.key

        LoadedList ll ->
            ll.key

        LoadedAlbum la ->
            la.key


withScrollPos : Viewport -> AlbumBootstrap -> AlbumBootstrap
withScrollPos rootDivViewport model =
    case model of
        Sizing _ ->
            model

        LoadingHomeLink _ ->
            model

        Loading _ ->
            model

        LoadError _ ->
            model

        LoadedAlbum la ->
            case la.albumPage of
                Thumbs th ->
                    let
                        oldVpInfo =
                            th.vpInfo
                    in
                    LoadedAlbum
                        { la
                            | albumPage = Thumbs { th | vpInfo = { oldVpInfo | rootDivViewport = Just rootDivViewport } }
                            , rootDivViewport = Just rootDivViewport
                        }

                FullImage fi ->
                    let
                        oldVpInfo =
                            fi.vpInfo
                    in
                    LoadedAlbum
                        { la
                            | albumPage = FullImage { fi | vpInfo = { oldVpInfo | rootDivViewport = Just rootDivViewport } }
                            , rootDivViewport = Just rootDivViewport
                        }

        LoadedList ll ->
            LoadedList { ll | rootDivViewport = Just rootDivViewport }


withPaths : AlbumBootstrap -> List String -> AlbumBootstrap
withPaths model paths =
    case model of
        Sizing sz ->
            Sizing { sz | paths = Just paths }

        LoadingHomeLink lh ->
            LoadingHomeLink { lh | paths = Just paths }

        Loading ld ->
            Loading { ld | paths = Just paths }

        LoadError _ ->
            model

        LoadedList ll ->
            LoadedList { ll | navState = NavInProgress }

        LoadedAlbum la ->
            LoadedAlbum { la | navState = NavInProgress }


withScroll : AlbumBootstrap -> Float -> AlbumBootstrap
withScroll model scroll =
    case model of
        Sizing sz ->
            Sizing { sz | scroll = Just scroll }

        LoadingHomeLink lh ->
            LoadingHomeLink { lh | scroll = Just scroll }

        Loading ld ->
            Loading { ld | scroll = Just scroll }

        LoadError _ ->
            model

        LoadedList _ ->
            model

        LoadedAlbum _ ->
            model


pathsToCmd : AlbumBootstrap -> Maybe (List String) -> Maybe AlbumBootstrapMsg
pathsToCmd model mPaths =
    case mPaths of
        Nothing ->
            log "pathsToCmd has no paths" Nothing

        Just paths ->
            case model of
                Sizing _ ->
                    Nothing

                LoadingHomeLink _ ->
                    Nothing

                Loading _ ->
                    Nothing

                LoadError _ ->
                    log "pathsToCmd LoadError, ignore" Nothing

                LoadedList ll ->
                    case ll.listPage of
                        AlbumListPage alp ->
                            --TODO maybe don't always prepend aTN here, only if at root?
                            --TODO I think it's okay to drop the scroll positions here, should only happen at initial load (?)
                            pathsToCmdImpl { bodyViewport = alp.bodyViewport, rootDivViewport = ll.rootDivViewport } (alp.albumList :: List.map Tuple.first alp.parents) paths

                LoadedAlbum la ->
                    pathsToCmdImpl (pageSize la.albumPage) (List.map Tuple.first la.parents) paths


pathsToCmdImpl : ViewportInfo -> List AlbumList -> List String -> Maybe AlbumBootstrapMsg
pathsToCmdImpl viewport parents paths =
    let
        mRoot =
            List.head <| List.reverse parents
    in
    case mRoot of
        Nothing ->
            log "pathsToCmdImpl has no root" Nothing

        Just root ->
            navFrom viewport root [] paths <| Just <| ViewList (AlbumListPage { albumList = root, bodyViewport = viewport.bodyViewport, parents = [] }) Nothing


scrollToCmd : AlbumBootstrap -> Maybe Float -> Maybe AlbumBootstrapMsg
scrollToCmd model scroll =
    let
        scrollCmd =
            Just <| ScheduleScroll scroll
    in
    case model of
        Sizing _ ->
            Nothing

        LoadingHomeLink _ ->
            Nothing

        Loading _ ->
            Nothing

        LoadError _ ->
            log "scrollToCmd LoadError, ignore" Nothing

        LoadedList _ ->
            scrollCmd

        LoadedAlbum _ ->
            scrollCmd


navFrom : ViewportInfo -> AlbumList -> List AlbumList -> List String -> Maybe AlbumBootstrapMsg -> Maybe AlbumBootstrapMsg
navFrom viewport root parents paths defcmd =
    case paths of
        [] ->
            log "navFrom has no paths" defcmd

        [ "#" ] ->
            log "navFrom has only # path" defcmd

        p1 :: ps ->
            let
                mChild =
                    findChild root p1

                newParents =
                    root :: parents
            in
            case mChild of
                Nothing ->
                    log ("navFrom can't find child " ++ p1) defcmd

                Just pChild ->
                    case pChild of
                        List albumList ->
                            navFrom viewport albumList newParents ps <| Just <| ViewList (AlbumListPage { albumList = albumList, bodyViewport = viewport.bodyViewport, parents = List.map (\p -> ( p, Nothing )) newParents }) Nothing

                        Leaf album ->
                            navForAlbum viewport album ps newParents


navForAlbum : ViewportInfo -> Album -> List String -> List AlbumList -> Maybe AlbumBootstrapMsg
navForAlbum vpInfo album ps newParents =
    let
        parentsNoScroll =
            List.map (\p -> ( p, Nothing )) newParents
    in
    case ps of
        [] ->
            Just <| ViewAlbum (Thumbs { album = album, vpInfo = vpInfo, justLoadedImages = Set.empty, readyToDisplayImages = Set.empty }) parentsNoScroll

        i :: _ ->
            case findImg [] album i of
                Nothing ->
                    log ("navForAlbum can't find image " ++ i) Nothing

                Just ( prevs, nAlbum ) ->
                    let
                        ( w, h ) =
                            fitImage nAlbum.imageFirst.srcSetFirst (floor vpInfo.bodyViewport.viewport.width) (floor vpInfo.bodyViewport.viewport.height)

                        ( progModel, progCmd ) =
                            progInit vpInfo.bodyViewport nAlbum.imageFirst w h
                    in
                    Just <|
                        Sequence
                            (ViewAlbum (FullImage { prevImgs = prevs, album = nAlbum, progModel = progModel, vpInfo = vpInfo, scroll = Nothing, dragInfo = Nothing }) parentsNoScroll)
                        <|
                            fromMaybe <|
                                Maybe.map (PageMsg << FullMsg) progCmd


findImg : List Image -> Album -> String -> Maybe ( List Image, Album )
findImg prevs album img =
    if album.imageFirst.altText == img then
        Just ( prevs, album )

    else
        case album.imageRest of
            [] ->
                Nothing

            imageNext :: imageRest ->
                findImg
                    (prevs ++ [ album.imageFirst ])
                    { album
                        | imageFirst = imageNext
                        , imageRest = imageRest
                    }
                    img


findChild : AlbumList -> String -> Maybe AlbumOrList
findChild containingList name =
    let
        f albumOrList =
            case albumOrList of
                List albumList ->
                    albumList.listTitle == name

                Leaf album ->
                    album.title == name
    in
    List.head <| List.filter f <| containingList.childFirst :: containingList.childRest


locFor : AlbumBootstrap -> AlbumBootstrap -> Maybe UrlChange
locFor oldModel newModel =
    let
        model =
            newModel

        entry =
            case oldModel of
                LoadedList ll ->
                    case newModel of
                        LoadedList ll2 ->
                            case ll.listPage == ll2.listPage of
                                True ->
                                    log "locFor LoadedList same" ModifyEntry

                                False ->
                                    log "locFor LoadedList dift" NewEntry

                        _ ->
                            log "locFor LoadedList -> something else" NewEntry

                LoadedAlbum la ->
                    case newModel of
                        LoadedAlbum la2 ->
                            case eqIgnoringVpInfo la.albumPage la2.albumPage && la.parents == la2.parents of
                                True ->
                                    log "locFor LoadedAlbum same" ModifyEntry

                                False ->
                                    log "locFor LoadedAlbum dift" NewEntry

                        _ ->
                            log "locFor LoadedAlbum -> something else" NewEntry

                _ ->
                    log "locFor something els -> *" NewEntry

        checkNavState state nav =
            case log "checkNavState" state of
                NavInProgress ->
                    Nothing

                NavInactive ->
                    nav

        newQorF meta modl fragment =
            case queryFor modl of
                "" ->
                    NewFragment meta fragment

                q ->
                    NewQuery meta { query = q, fragment = Just fragment }
    in
    log "locFor" <|
        case model of
            LoadedAlbum la ->
                checkNavState la.navState <|
                    Just <|
                        newQorF { entry = entry, key = la.key }
                            model
                        <|
                            hashForAlbum model la.albumPage <|
                                List.map Tuple.first la.parents

            LoadedList ll ->
                checkNavState ll.navState <|
                    Just <|
                        newQorF { entry = entry, key = ll.key }
                            model
                        <|
                            hashForList model ll.listPage

            _ ->
                Nothing


queryFor : AlbumBootstrap -> String
queryFor model =
    let
        queryForPos pos =
            Maybe.withDefault "" <| Maybe.map (\p -> "s=" ++ String.fromFloat p) pos
    in
    case model of
        Sizing _ ->
            ""

        LoadingHomeLink _ ->
            ""

        Loading _ ->
            ""

        LoadError _ ->
            ""

        LoadedAlbum la ->
            queryForPos <| Maybe.map scrollPosOf la.rootDivViewport

        LoadedList ll ->
            queryForPos <| Maybe.map scrollPosOf ll.rootDivViewport


hashForList : AlbumBootstrap -> AlbumListPage -> String
hashForList model (AlbumListPage alp) =
    if List.isEmpty alp.parents then
        hashFromAlbumPath model [ "" ] []

    else
        hashFromAlbumPath model [ alp.albumList.listTitle ] <| List.map Tuple.first alp.parents


hashForAlbum : AlbumBootstrap -> AlbumPage -> List AlbumList -> String
hashForAlbum model albumPage parents =
    let
        titles =
            case albumPage of
                Thumbs th ->
                    [ th.album.title ]

                FullImage fi ->
                    [ fi.album.title, fi.album.imageFirst.altText ]
    in
    hashFromAlbumPath model titles parents


hashFromAlbumPath : AlbumBootstrap -> List String -> List AlbumList -> String
hashFromAlbumPath model titles parents =
    String.concat
        (List.intersperse "/"
            (List.map
                percentEncode
                (List.append
                    (List.map
                        (\p -> p.listTitle)
                        (List.drop 1 (List.reverse parents))
                    )
                    titles
                )
            )
        )


scrollToTop : Cmd AlbumBootstrapMsg
scrollToTop =
    Task.attempt
        (\result ->
            case result of
                Ok () ->
                    ScrollSucceeded

                Err e ->
                    ScrollFailed rootDivId
        )
    <|
        setViewportOf rootDivId 0 0


updateImageResult : AlbumBootstrap -> String -> UrlLoadState -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
updateImageResult model url result =
    case model of
        LoadedAlbum la ->
            case la.albumPage of
                Thumbs th ->
                    let
                        newModel =
                            justLoadedReadyToDisplayNextState th url result

                        urls =
                            AlbumPage.urlsToGet newModel
                    in
                    ( LoadedAlbum
                        { la
                            | albumPage = newModel
                            , pendingUrls =
                                Dict.union (Dict.fromList [ ( url, result ) ]) <|
                                    Dict.union la.pendingUrls <|
                                        dictWithValues urls UrlRequested
                        }
                    , Cmd.batch
                        [ getUrls la.pendingUrls urls
                        , urlNextState url result
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


justLoadedReadyToDisplayNextState th url result =
    case result of
        JustCompleted ->
            Thumbs { th | justLoadedImages = Set.insert url th.justLoadedImages }

        ReadyToDisplay ->
            Thumbs { th | justLoadedImages = Set.remove url th.justLoadedImages, readyToDisplayImages = Set.insert url th.readyToDisplayImages }

        _ ->
            Thumbs th


urlNextState : String -> UrlLoadState -> Cmd AlbumBootstrapMsg
urlNextState url result =
    case result of
        JustCompleted ->
            Delay.after 100 Millisecond <| ImageReadyToDisplay url

        _ ->
            Cmd.none


getUrls : Dict String UrlLoadState -> Set String -> Cmd AlbumBootstrapMsg
getUrls existingUrls newUrls =
    Cmd.batch <| List.map getUrl <| Set.toList <| Set.diff newUrls <| Set.fromList <| keys existingUrls


getUrl : String -> Cmd AlbumBootstrapMsg
getUrl url =
    Http.get
        { url = encodePath url
        , expect = expectWhatever <| decodeUrlResult url
        }


decodeUrlResult : String -> Result Http.Error () -> AlbumBootstrapMsg
decodeUrlResult origUrl result =
    case result of
        Ok _ ->
            ImageLoaded origUrl

        Err e ->
            ImageFailed origUrl e


subscriptions : AlbumBootstrap -> Sub AlbumBootstrapMsg
subscriptions model =
    case model of
        LoadedAlbum la ->
            let
                showParent =
                    case la.parents of
                        [] ->
                            NoBootstrap

                        ( parent, scroll ) :: grandParents ->
                            ViewList (AlbumListPage { albumList = parent, bodyViewport = (pageSize la.albumPage).bodyViewport, parents = grandParents }) scroll
            in
            Sub.batch
                [ AlbumPage.subscriptions la.albumPage PageMsg showParent
                , onResize <| newSize <| (pageSize la.albumPage).bodyViewport
                ]

        LoadedList ll ->
            case ll.listPage of
                AlbumListPage alp ->
                    case alp.parents of
                        [] ->
                            onResize <| newSize alp.bodyViewport

                        ( parent, scroll ) :: grandParents ->
                            let
                                upParent =
                                    onEscape
                                        (ViewList (AlbumListPage { alp | albumList = parent, parents = grandParents }) scroll)
                                        NoBootstrap
                            in
                            Sub.batch [ upParent, onResize <| newSize alp.bodyViewport ]

        LoadingHomeLink lh ->
            onResize <| newSize lh.bodyViewport

        Loading ld ->
            Sub.batch
                [ onResize <| newSize ld.bodyViewport
                , Http.track albumJson LoadAlbumProgress
                ]

        LoadError _ ->
            Sub.none

        Sizing _ ->
            Sub.none


newSize : Viewport -> Int -> Int -> AlbumBootstrapMsg
newSize v x y =
    Resize <| viewportWithNewSize v x y



--TODO won't other things about the viewport change if the resize causes the page to reflow???
--TODO why int vs. float confusion?


viewportWithNewSize : Viewport -> Int -> Int -> Viewport
viewportWithNewSize oldViewport newWidth newHeight =
    let
        ov =
            oldViewport.viewport

        newViewport =
            { ov | width = toFloat newWidth, height = toFloat newHeight }
    in
    { oldViewport | viewport = newViewport }


pageSize : AlbumPage -> ViewportInfo
pageSize albumPage =
    case albumPage of
        Thumbs th ->
            th.vpInfo

        FullImage fi ->
            fi.vpInfo


scrollPosOf : Viewport -> Float
scrollPosOf viewport =
    viewport.viewport.y


view : AlbumBootstrap -> Document AlbumBootstrapMsg
view albumBootstrap =
    let
        title =
            case albumBootstrap of
                Sizing _ ->
                    "Album Starting"

                LoadingHomeLink _ ->
                    "Home Loading ..."

                Loading ld ->
                    viewProgress "Album Loading" ld.progress

                LoadError _ ->
                    "Error Loading Album"

                LoadedList ll ->
                    case ll.listPage of
                        AlbumListPage alp ->
                            alp.albumList.listTitle

                LoadedAlbum la ->
                    titleOf la.albumPage
    in
    { title = title
    , body = [ viewImpl albumBootstrap |> toUnstyled ]
    }


viewImpl : AlbumBootstrap -> Html AlbumBootstrapMsg
viewImpl albumBootstrap =
    case albumBootstrap of
        Sizing _ ->
            text "Album Starting"

        LoadingHomeLink _ ->
            text "Home Loading ..."

        Loading ld ->
            text <| viewProgress "Album Loading" ld.progress

        LoadError le ->
            let
                eStr =
                    case le.error of
                        BadUrl s ->
                            "bad url: " ++ s

                        Timeout ->
                            "timeout"

                        NetworkError ->
                            "network error"

                        BadStatus code ->
                            "bad status: " ++ String.fromInt code

                        BadBody s ->
                            "bad payload: " ++ s
            in
            text ("Error Loading Album: " ++ eStr)

        LoadedAlbum la ->
            withHomeLink la.home la.flags <|
                AlbumPage.view
                    la.albumPage
                    throttledScrolledTo
                    (viewList
                        albumBootstrap
                        (pageSize la.albumPage).bodyViewport
                        -- currently scrolled thing is the album;
                        -- don't want to save that anywhere in the list of parents
                        la.parents
                    )
                    PageMsg
                    (List.map Tuple.first la.parents)
                    la.flags

        LoadedList ll ->
            case ll.listPage of
                AlbumListPage alp ->
                    withHomeLink ll.home ll.flags <|
                        AlbumListPage.view
                            (AlbumListPage alp)
                            (viewList
                                albumBootstrap
                                alp.bodyViewport
                                (( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport ) :: alp.parents)
                            )
                            (\album ->
                                ViewAlbum
                                    (Thumbs { album = album, vpInfo = { bodyViewport = alp.bodyViewport, rootDivViewport = ll.rootDivViewport }, justLoadedImages = Set.empty, readyToDisplayImages = Set.empty })
                                <|
                                    ( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport )
                                        :: alp.parents
                            )
                            throttledScrolledTo
                            ll.flags


viewProgress : String -> Maybe Progress -> String
viewProgress prefix mProgress =
    let
        pct num denom =
            (String.fromInt <| Basics.round <| 100 * toFloat num / toFloat denom) ++ "%"
    in
    case mProgress of
        Nothing ->
            prefix

        Just progress ->
            case progress of
                Sending s ->
                    prefix ++ ": sent " ++ pct s.sent s.size

                Receiving r ->
                    case r.size of
                        Nothing ->
                            prefix ++ ": " ++ String.fromInt r.received ++ " bytes received"

                        Just size ->
                            prefix ++ ": received " ++ pct r.received size


viewList : AlbumBootstrap -> Viewport -> List ( AlbumList, Maybe Float ) -> AlbumList -> AlbumBootstrapMsg
viewList oldModel viewport parents list =
    ViewList
        (AlbumListPage
            { albumList = list
            , bodyViewport = viewport
            , parents =
                dropThroughPred
                    (\( p, _ ) -> p == list)
                    parents
            }
        )
        (if List.member list (List.map Tuple.first parents) then
            --we're navigating up to a parent
            --look up and use saved scroll position of that parent
            case List.head <| List.filter (\e -> list == Tuple.first e) parents of
                Just p ->
                    Tuple.second p

                Nothing ->
                    Nothing

         else
            --we're navigating down to a child; since we don't save
            --scroll positions of children, don't declare an explicit
            --target scroll position
            Nothing
        )


withHomeLink : Maybe String -> AlbumBootstrapFlags -> Html AlbumBootstrapMsg -> Html AlbumBootstrapMsg
withHomeLink home flags basePage =
    case home of
        Just h ->
            div
                []
                [ basePage
                , a
                    [ href h
                    , styles <|
                        [ top <| px 0
                        , left <| px 0
                        , textDecoration none
                        ]
                            ++ navBoxStyles
                            ++ [ rootPos flags ]
                    ]
                    [ text "⌂" ]
                ]

        Nothing ->
            basePage
