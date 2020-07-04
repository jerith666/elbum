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
import Debounce exposing (..)
import Delay exposing (..)
import Dict exposing (..)
import FullImagePage exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events
import Http exposing (..)
import RouteUrl exposing (..)
import Set exposing (..)
import Task exposing (..)
import Time exposing (..)
import Url exposing (..)
import Utils.AlbumUtils exposing (..)
import Utils.DebugSupport exposing (debugString, log)
import Utils.HttpUtils exposing (..)
import Utils.KeyboardUtils exposing (onEscape)
import Utils.ListUtils exposing (..)
import Utils.LocationUtils exposing (..)
import Utils.ResultUtils exposing (..)
import Utils.TouchUtils as TU exposing (..)
import Utils.ViewportUtils exposing (..)


type MainAlbumModel
    = Sizing
        { key : Key
        , flags : MainAlbumFlags
        , albumPathsAfterLoad : Maybe (List String)
        , scrollToAfterLoad : Maybe Float
        }
    | LoadingHomeLink
        { key : Key
        , bodyViewport : Viewport
        , flags : MainAlbumFlags
        , albumPathsAfterLoad : Maybe (List String)
        , scrollToAfterLoad : Maybe Float
        }
    | Loading
        { key : Key
        , bodyViewport : Viewport
        , progress : Maybe Progress
        , flags : MainAlbumFlags
        , home : Maybe String
        , albumPathsAfterLoad : Maybe (List String)
        , scrollToAfterLoad : Maybe Float
        }
    | LoadError
        { key : Key
        , flags : MainAlbumFlags
        , error : Http.Error
        }
    | LoadedList
        { key : Key
        , listPage : AlbumListPage
        , flags : MainAlbumFlags
        , home : Maybe String
        , pendingUrls : Dict String UrlLoadState
        , rootDivViewport : Maybe Viewport
        , navState : PostLoadNavState
        , debounce : Debounce Viewport
        }
    | LoadedAlbum
        { key : Key
        , albumPage : AlbumPage
        , parents : List ( AlbumList, Maybe Float )
        , flags : MainAlbumFlags
        , home : Maybe String
        , pendingUrls : Dict String UrlLoadState
        , rootDivViewport : Maybe Viewport
        , navState : PostLoadNavState
        , debounce : Debounce Viewport
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


type MainAlbumMsg
    = Bootstrap BootstrapMsg
    | Meta MetaMsg
    | Album AlbumMsg
    | Scroll ScrollMsg
    | General GeneralMsg


type MetaMsg
    = Sequence MainAlbumMsg (List MainAlbumMsg)
    | SequenceCmd (Cmd MainAlbumMsg) (List (Cmd MainAlbumMsg))
    | NoBootstrap


type BootstrapMsg
    = YesHome String
    | NoHome Http.Error
    | LoadAlbumProgress Progress
    | YesAlbum AlbumOrList
    | NoAlbum Http.Error


type AlbumMsg
    = SetAlbumPathFromUrl (List String)
    | PageMsg AlbumPage.AlbumPageMsg
    | ViewList AlbumListPage (Maybe Float)
    | ViewAlbum AlbumPage (List ( AlbumList, Maybe Float ))
    | ImageLoaded String
    | ImageReadyToDisplay String
    | ImageFailed String Http.Error
    | NavCompletedLocally


type ScrollMsg
    = ScrolledTo Viewport
    | RawScrolledTo Viewport
    | DebounceMsg Debounce.Msg
    | EnactScrollFromUrl (Maybe Float)
    | SetScrollFromUrl Float


type GeneralMsg
    = Resize Viewport
    | LinkClicked UrlRequest


main : RouteUrlProgram MainAlbumFlags MainAlbumModel MainAlbumMsg
main =
    RouteUrl.anchorManagedProgramWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = General << LinkClicked
        , delta2url = locFor
        , location2messages = navToMsg
        , makeAnchor = makeAnchor
        }


makeAnchor : String -> Maybe MainAlbumMsg -> List (Attribute MainAlbumMsg) -> List (Html MainAlbumMsg) -> Html MainAlbumMsg
makeAnchor url onClickMsg attrs =
    case onClickMsg of
        Nothing ->
            a (href url :: attrs)

        Just m ->
            a (href url :: Html.Styled.Events.onClick m :: attrs)


init : MainAlbumFlags -> Key -> ( MainAlbumModel, Cmd MainAlbumMsg )
init flags key =
    ( Sizing { key = key, flags = flags, albumPathsAfterLoad = Nothing, scrollToAfterLoad = Nothing }
    , Task.perform (General << Resize) getViewport
    )


update : MainAlbumMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
update msg model =
    case log "update msg" msg of
        Bootstrap albumBootstrapMsg ->
            updateBootstrap albumBootstrapMsg model

        Meta albumMetaMsg ->
            updateMeta albumMetaMsg model

        Album albumMsg ->
            updateAlbum albumMsg model

        Scroll scrollMsg ->
            updateScroll scrollMsg model

        General generalMsg ->
            updateGeneral generalMsg model


updateGeneral : GeneralMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
updateGeneral generalMsg model =
    case generalMsg of
        Resize viewport ->
            case model of
                Sizing sz ->
                    ( LoadingHomeLink
                        { key = sz.key
                        , bodyViewport = log "window size set" viewport
                        , flags = sz.flags
                        , albumPathsAfterLoad = sz.albumPathsAfterLoad
                        , scrollToAfterLoad = sz.scrollToAfterLoad
                        }
                    , Cmd.map Bootstrap <| Http.get { url = "home", expect = expectString <| either NoHome YesHome }
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
                            , Cmd.map (Album << PageMsg) getImgPosition
                            )

                LoadedList ll ->
                    case ll.listPage of
                        AlbumListPage alp ->
                            ( LoadedList { ll | listPage = AlbumListPage { alp | bodyViewport = viewport } }
                            , Cmd.none
                            )

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
                            --( log ("ignoring unexpected internal url request not for home url (" ++ (Maybe.withDefault "home not set" <| homeOf model) ++ ") " ++ toString url) model, Cmd.none )
                            ( model, pushUrl (keyOf model) <| toString url )

                External url ->
                    --home link should be only external link in our app
                    ( model, load url )


updateBootstrap : BootstrapMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
updateBootstrap bootstrapMsg model =
    case bootstrapMsg of
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
                                        , listPage =
                                            AlbumListPage
                                                { albumList = albumList
                                                , bodyViewport = ld.bodyViewport
                                                , parents = []
                                                }
                                        , flags = ld.flags
                                        , home = ld.home
                                        , pendingUrls = Dict.empty
                                        , rootDivViewport = Nothing
                                        , navState = NavInactive
                                        , debounce = Debounce.init
                                        }

                                pathsThenScroll =
                                    toCmd <|
                                        sequence (pathsToCmd newModel ld.albumPathsAfterLoad) <|
                                            fromMaybe <|
                                                scrollToCmd newModel ld.scrollToAfterLoad
                            in
                            ( newModel
                            , pathsThenScroll
                            )

                        Leaf album ->
                            let
                                albumPage =
                                    Thumbs
                                        { album = album
                                        , vpInfo =
                                            { bodyViewport = ld.bodyViewport
                                            , rootDivViewport = Nothing
                                            }
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
                                        , debounce = Debounce.init
                                        }

                                pathsThenScroll =
                                    toCmd <|
                                        sequence (pathsToCmd newModel ld.albumPathsAfterLoad) <|
                                            fromMaybe <|
                                                scrollToCmd newModel ld.scrollToAfterLoad
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


updateAlbum : AlbumMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
updateAlbum albumMsg model =
    case albumMsg of
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
                    ( LoadedAlbum
                        { la
                            | albumPage = newPage
                            , pendingUrls = Dict.union newPendingUrls <| dictWithValues urls UrlRequested
                        }
                    , Cmd.batch
                        [ getUrls newPendingUrls urls
                        , Cmd.map (Album << PageMsg) newPageCmd
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
                        , debounce = debounceOf model
                        }

                scrollCmd =
                    case maybeScroll of
                        Just pos ->
                            Task.attempt (always NoBootstrap) <| setViewportOf rootDivId 0 pos

                        Nothing ->
                            scrollToTop NoBootstrap <| always NoBootstrap

                title =
                    case albumListPage of
                        AlbumListPage alp ->
                            alp.albumList.listTitle
            in
            ( newModel
            , Cmd.map Meta scrollCmd
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
                        , debounce = debounceOf model
                        }

                getImgPos =
                    case albumPage of
                        Thumbs _ ->
                            Cmd.none

                        FullImage _ ->
                            Cmd.map PageMsg getImgPosition
            in
            ( newModel
            , Cmd.batch
                [ Cmd.map Meta <| scrollToTop NoBootstrap <| always NoBootstrap
                , getUrls Dict.empty urls
                , Cmd.map Album getImgPos
                ]
            )

        SetAlbumPathFromUrl paths ->
            -- as with SetScrollFromUrl, 2 cases: early loading, save for later; and already loaded, apply now
            -- but no hacky delay here
            ( withAlbumPathsAfterLoad model paths
            , toCmd <| Maybe.withDefault (Meta NoBootstrap) <| pathsToCmd model <| Just paths
            )

        NavCompletedLocally ->
            ( withNavComplete model, Cmd.none )


updateScroll : ScrollMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
updateScroll scrollMsg model =
    case scrollMsg of
        RawScrolledTo viewport ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig viewport <| debounceOf model
            in
            ( withDebounce debounce model, Cmd.map Scroll cmd )

        DebounceMsg dMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast <| toCmd << ScrolledTo) dMsg <| debounceOf model
            in
            ( withDebounce debounce model, Cmd.map Scroll cmd )

        ScrolledTo viewport ->
            ( withScrollPos (log "ScrolledTo: " viewport) model, Cmd.none )

        EnactScrollFromUrl scroll ->
            ( model
            , Maybe.withDefault Cmd.none <|
                Maybe.map
                    (\s ->
                        Task.attempt
                            (always (Meta NoBootstrap))
                        <|
                            setViewportOf rootDivId 0 <|
                                log "startup scroll to" s
                    )
                    scroll
            )

        SetScrollFromUrl s ->
            ( -- case 1: we're early in the loading process, the page isn't in a state where we can scroll right now
              -- so, store the target we want to scroll to in the model for later enactment (in YesAlbum case, above)
              withScrollToAfterLoad model s
              -- case 2: we've loaded the album or album list and the page is in a state where we can scroll right now
              -- so, create a command to enact that scroll (but not a direct setViewportOf cmd, indirect through
              -- EnactScrollFromUrl ... probably for timing reasons, exact details would have to be dug up from git logs)
            , toCmd <| Maybe.withDefault (Meta NoBootstrap) <| scrollToCmd model <| Just s
            )


updateMeta : MetaMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
updateMeta albumMetaMsg model =
    case albumMetaMsg of
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
                                            update (Meta <| Sequence rs1 rss) r1Model
                                    in
                                    ( rsModel, toCmd <| Meta <| SequenceCmd r1Cmds [ rsCmds ] )
                    in
                    ( rModel, toCmd <| Meta <| SequenceCmd (log ("sequence msg " ++ debugString next ++ " (cont'd) produces cmd") nextCmd) [ rCmds ] )

        SequenceCmd next rest ->
            let
                cmds =
                    case rest of
                        [] ->
                            log "sequenced cmd: last" next

                        r1 :: rs ->
                            Cmd.batch [ log "sequenced cmd: next" next, toCmd <| Meta <| SequenceCmd r1 rs ]
            in
            ( model, cmds )

        NoBootstrap ->
            ( model, Cmd.none )


debounceOf model =
    case model of
        LoadedList ll ->
            ll.debounce

        LoadedAlbum la ->
            la.debounce

        _ ->
            Debounce.init


withDebounce debounce model =
    case model of
        LoadedList ll ->
            LoadedList { ll | debounce = debounce }

        LoadedAlbum la ->
            LoadedAlbum { la | debounce = debounce }

        _ ->
            model


{-| Safari throws an exception if a webapp calls replaceState more than 100
times in 30 seconds. This exception basically kills the Elm event loop and
renders the entire app unresponsive. So, we have to use a debouncer library to
throttle rate at which ScrolledTo messages are processed, because they can cause
very frequent updates to the ?s=NNN query param in our url.

100/30 sec works out to one event every 0.3s. Throttling to exactly that isn't
enough, we have to give a bit of headroom.
<https://bugs.webkit.org/show_bug.cgi?id=156115>

-}
debounceConfig =
    { strategy = soon 400, transform = DebounceMsg }


sequence : Maybe MainAlbumMsg -> List MainAlbumMsg -> MainAlbumMsg
sequence mm1 ms =
    case mm1 of
        Nothing ->
            case List.tail ms of
                Nothing ->
                    case List.head ms of
                        Nothing ->
                            Meta NoBootstrap

                        Just ms1 ->
                            ms1

                Just mst ->
                    sequence (List.head ms) mst

        Just m1 ->
            Meta <| Sequence m1 ms


gotHome lh home =
    ( Loading
        { key = lh.key
        , bodyViewport = lh.bodyViewport
        , progress = Nothing
        , flags = lh.flags
        , home = home
        , albumPathsAfterLoad = lh.albumPathsAfterLoad
        , scrollToAfterLoad = lh.scrollToAfterLoad
        }
    , Cmd.map Bootstrap <|
        Http.request
            { method = "GET"
            , headers = []
            , url = albumJson
            , body = emptyBody
            , expect = expectJson (either NoAlbum YesAlbum) jsonDecAlbumOrList
            , timeout = Nothing
            , tracker = Just albumJson
            }
    )


navToMsg : Url -> List MainAlbumMsg
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
                    [ Album <| SetAlbumPathFromUrl paths ]

        queryMsgs =
            case parsedQuery of
                Err _ ->
                    []

                Ok scroll ->
                    Meta NoBootstrap
                        -- hack delay
                        :: (fromMaybe <|
                                Maybe.map (Scroll << SetScrollFromUrl) (scroll |> Maybe.andThen String.toFloat)
                           )
    in
    case hashMsgs ++ queryMsgs of
        [] ->
            []

        [ c ] ->
            [ c ]

        c1 :: cs ->
            [ Meta <| Sequence c1 cs ]


flagsOf : MainAlbumModel -> MainAlbumFlags
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


homeOf : MainAlbumModel -> Maybe String
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


keyOf : MainAlbumModel -> Key
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


withScrollPos : Viewport -> MainAlbumModel -> MainAlbumModel
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


withAlbumPathsAfterLoad : MainAlbumModel -> List String -> MainAlbumModel
withAlbumPathsAfterLoad model albumPathsAfterLoad =
    case model of
        Sizing sz ->
            Sizing { sz | albumPathsAfterLoad = Just albumPathsAfterLoad }

        LoadingHomeLink lh ->
            LoadingHomeLink { lh | albumPathsAfterLoad = Just albumPathsAfterLoad }

        Loading ld ->
            Loading { ld | albumPathsAfterLoad = Just albumPathsAfterLoad }

        LoadError _ ->
            model

        LoadedList ll ->
            LoadedList { ll | navState = NavInProgress }

        LoadedAlbum la ->
            LoadedAlbum { la | navState = NavInProgress }


withScrollToAfterLoad : MainAlbumModel -> Float -> MainAlbumModel
withScrollToAfterLoad model scrollToAfterLoad =
    case model of
        Sizing sz ->
            Sizing { sz | scrollToAfterLoad = Just scrollToAfterLoad }

        LoadingHomeLink lh ->
            LoadingHomeLink { lh | scrollToAfterLoad = Just scrollToAfterLoad }

        Loading ld ->
            Loading { ld | scrollToAfterLoad = Just scrollToAfterLoad }

        LoadError _ ->
            model

        LoadedList _ ->
            model

        LoadedAlbum _ ->
            model


withNavComplete : MainAlbumModel -> MainAlbumModel
withNavComplete model =
    case model of
        Sizing _ ->
            model

        LoadingHomeLink _ ->
            model

        Loading _ ->
            model

        LoadError _ ->
            model

        LoadedList ll ->
            LoadedList { ll | navState = NavInactive }

        LoadedAlbum la ->
            LoadedAlbum { la | navState = NavInactive }


pathsToCmd : MainAlbumModel -> Maybe (List String) -> Maybe MainAlbumMsg
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
                            pathsToCmdImpl model
                                { bodyViewport = alp.bodyViewport, rootDivViewport = ll.rootDivViewport }
                                (alp.albumList :: List.map Tuple.first alp.parents)
                                paths

                LoadedAlbum la ->
                    pathsToCmdImpl model (pageSize la.albumPage) (List.map Tuple.first la.parents) paths


pathsToCmdImpl : MainAlbumModel -> ViewportInfo -> List AlbumList -> List String -> Maybe MainAlbumMsg
pathsToCmdImpl model viewport parents paths =
    let
        mRoot =
            List.head <| List.reverse parents
    in
    case mRoot of
        Nothing ->
            log "pathsToCmdImpl has no root" Nothing

        Just root ->
            navFrom model viewport root [] paths <|
                Album <|
                    ViewList
                        (AlbumListPage { albumList = root, bodyViewport = viewport.bodyViewport, parents = [] })
                        Nothing


scrollToCmd : MainAlbumModel -> Maybe Float -> Maybe MainAlbumMsg
scrollToCmd model scrollToAfterLoad =
    let
        scrollCmd =
            Just <| Scroll <| EnactScrollFromUrl scrollToAfterLoad
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


navFrom : MainAlbumModel -> ViewportInfo -> AlbumList -> List AlbumList -> List String -> MainAlbumMsg -> Maybe MainAlbumMsg
navFrom model viewport root parents paths defcmd =
    case paths of
        [] ->
            log "navFrom has no paths" <| Just defcmd

        [ "#" ] ->
            log "navFrom has only # path" <| Just defcmd

        p1 :: ps ->
            let
                mChild =
                    findChild root p1

                newParents =
                    root :: parents
            in
            case mChild of
                Nothing ->
                    log ("navFrom can't find child " ++ p1) <| Just defcmd

                Just pChild ->
                    case pChild of
                        List albumList ->
                            navFrom model viewport albumList newParents ps <|
                                Album <|
                                    ViewList
                                        (AlbumListPage
                                            { albumList = albumList
                                            , bodyViewport = viewport.bodyViewport
                                            , parents = List.map (\p -> ( p, Nothing )) newParents
                                            }
                                        )
                                        Nothing

                        Leaf album ->
                            navForAlbum model viewport album ps newParents


navForAlbum : MainAlbumModel -> ViewportInfo -> Album -> List String -> List AlbumList -> Maybe MainAlbumMsg
navForAlbum model vpInfo album ps newParents =
    let
        parentsNoScroll =
            List.map (\p -> ( p, Nothing )) newParents
    in
    case ps of
        [] ->
            Just <|
                Album <|
                    ViewAlbum
                        (Thumbs
                            { album = album
                            , vpInfo = vpInfo
                            , justLoadedImages = Set.empty
                            , readyToDisplayImages = Set.empty
                            }
                        )
                        parentsNoScroll

        i :: _ ->
            case findImg [] album i of
                Nothing ->
                    log ("navForAlbum can't find image " ++ i) Nothing

                Just ( prevs, nAlbum ) ->
                    let
                        ( w, h ) =
                            fitImage
                                nAlbum.imageFirst.srcSetFirst
                                (floor vpInfo.bodyViewport.viewport.width)
                                (floor vpInfo.bodyViewport.viewport.height)

                        ( progModel, progCmd ) =
                            progInit vpInfo.bodyViewport nAlbum.imageFirst w h

                        nonLocalCmd =
                            Just <|
                                Meta <|
                                    Sequence
                                        (Album <|
                                            ViewAlbum
                                                (FullImage
                                                    { prevImgs = prevs
                                                    , album = nAlbum
                                                    , progModel = progModel
                                                    , vpInfo = vpInfo
                                                    , scroll = Nothing
                                                    , touchState = TU.init
                                                    , imgPosition = Nothing
                                                    , thumbLoadState = SomeMissing
                                                    }
                                                )
                                                parentsNoScroll
                                        )
                                    <|
                                        fromMaybe <|
                                            Maybe.map (Album << PageMsg << FullMsg) progCmd
                    in
                    case model of
                        LoadedAlbum la ->
                            case la.albumPage of
                                Thumbs t ->
                                    case t.album == album of
                                        True ->
                                            Just <|
                                                Meta <|
                                                    Sequence
                                                        (Album <| PageMsg <| View prevs nAlbum.imageFirst nAlbum.imageRest)
                                                        [ Album NavCompletedLocally ]

                                        False ->
                                            nonLocalCmd

                                _ ->
                                    nonLocalCmd

                        LoadedList ll ->
                            case ll.listPage of
                                AlbumListPage alp ->
                                    case List.member (Leaf album) (alp.albumList.childFirst :: alp.albumList.childRest) of
                                        True ->
                                            Just <|
                                                Meta <|
                                                    Sequence
                                                        (Album <|
                                                            ViewAlbum
                                                                (Thumbs
                                                                    { album = album
                                                                    , vpInfo = { bodyViewport = alp.bodyViewport, rootDivViewport = ll.rootDivViewport }
                                                                    , justLoadedImages = Set.empty
                                                                    , readyToDisplayImages = Set.empty
                                                                    }
                                                                )
                                                            <|
                                                                ( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport )
                                                                    :: alp.parents
                                                        )
                                                        [ Album NavCompletedLocally ]

                                        False ->
                                            nonLocalCmd

                        _ ->
                            nonLocalCmd


locFor : MainAlbumModel -> MainAlbumModel -> Maybe UrlChange
locFor oldModel newModel =
    let
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
                    log "locFor something else -> *" NewEntry

        checkNavState state nav =
            case log "checkNavState" state of
                NavInProgress ->
                    Nothing

                NavInactive ->
                    nav

        newQorF meta modl fragment =
            case entry of
                NewEntry ->
                    -- always drop scroll information when moving to a new page
                    -- otherwise it's incorrectly carried forward from e.g. album list to album pages
                    NewQuery meta { query = "", fragment = Just fragment }

                ModifyEntry ->
                    case queryFor modl of
                        Nothing ->
                            NewFragment meta fragment

                        Just q ->
                            NewQuery meta { query = q, fragment = Just fragment }
    in
    log "locFor" <|
        case newModel of
            LoadedAlbum la ->
                checkNavState la.navState <|
                    Just <|
                        newQorF { entry = entry, key = la.key }
                            newModel
                        <|
                            hashForAlbum la.albumPage <|
                                List.map Tuple.first la.parents

            LoadedList ll ->
                checkNavState ll.navState <|
                    Just <|
                        newQorF { entry = entry, key = ll.key }
                            newModel
                        <|
                            hashForList ll.listPage

            _ ->
                Nothing


queryFor : MainAlbumModel -> Maybe String
queryFor model =
    let
        queryForPos pos =
            Maybe.map (\p -> "s=" ++ String.fromFloat p) pos
    in
    case model of
        Sizing _ ->
            Nothing

        LoadingHomeLink _ ->
            Nothing

        Loading _ ->
            Nothing

        LoadError _ ->
            Nothing

        LoadedAlbum la ->
            queryForPos <| Maybe.map scrollPosOf la.rootDivViewport

        LoadedList ll ->
            queryForPos <| Maybe.map scrollPosOf ll.rootDivViewport


updateImageResult : MainAlbumModel -> String -> UrlLoadState -> ( MainAlbumModel, Cmd MainAlbumMsg )
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
            Thumbs
                { th | justLoadedImages = Set.insert url th.justLoadedImages }

        ReadyToDisplay ->
            Thumbs
                { th
                    | justLoadedImages = Set.remove url th.justLoadedImages
                    , readyToDisplayImages = Set.insert url th.readyToDisplayImages
                }

        _ ->
            Thumbs th


urlNextState : String -> UrlLoadState -> Cmd MainAlbumMsg
urlNextState url result =
    case result of
        JustCompleted ->
            Delay.after 100 Millisecond <| (Album <| ImageReadyToDisplay url)

        _ ->
            Cmd.none


getUrls : Dict String UrlLoadState -> Set String -> Cmd MainAlbumMsg
getUrls existingUrls newUrls =
    Cmd.map Album <|
        Cmd.batch <|
            List.map (\url -> getUrl (decodeUrlResult url) url) <|
                Set.toList <|
                    Set.diff newUrls <|
                        Set.fromList <|
                            keys existingUrls


decodeUrlResult : String -> Result Http.Error () -> AlbumMsg
decodeUrlResult origUrl result =
    either (ImageFailed origUrl) (always <| ImageLoaded origUrl) result


subscriptions : MainAlbumModel -> Sub MainAlbumMsg
subscriptions model =
    case model of
        LoadedAlbum la ->
            let
                showParent =
                    case la.parents of
                        [] ->
                            Meta NoBootstrap

                        ( parent, scroll ) :: grandParents ->
                            Album <|
                                ViewList
                                    (AlbumListPage
                                        { albumList = parent
                                        , bodyViewport = (pageSize la.albumPage).bodyViewport
                                        , parents = grandParents
                                        }
                                    )
                                    scroll
            in
            Sub.batch
                [ AlbumPage.subscriptions la.albumPage (Album << PageMsg) showParent
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
                                        (Album <|
                                            ViewList
                                                (AlbumListPage { alp | albumList = parent, parents = grandParents })
                                                scroll
                                        )
                                        (Meta NoBootstrap)
                            in
                            Sub.batch [ upParent, onResize <| newSize alp.bodyViewport ]

        LoadingHomeLink lh ->
            onResize <| newSize lh.bodyViewport

        Loading ld ->
            Sub.batch
                [ onResize <| newSize ld.bodyViewport
                , Http.track albumJson (Bootstrap << LoadAlbumProgress)
                ]

        LoadError _ ->
            Sub.none

        Sizing _ ->
            Sub.none


newSize : Viewport -> Int -> Int -> MainAlbumMsg
newSize v x y =
    General <| Resize <| viewportWithNewSize v x y


view : MainAlbumModel -> (MainAlbumMsg -> List (Attribute MainAlbumMsg) -> List (Html MainAlbumMsg) -> Html MainAlbumMsg) -> Document MainAlbumMsg
view albumBootstrap a =
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
    , body = [ viewImpl albumBootstrap a |> toUnstyled ]
    }


viewImpl : MainAlbumModel -> (MainAlbumMsg -> List (Attribute MainAlbumMsg) -> List (Html MainAlbumMsg) -> Html MainAlbumMsg) -> Html MainAlbumMsg
viewImpl albumBootstrap a =
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
                    a
                    (Scroll << RawScrolledTo)
                    (viewList
                        albumBootstrap
                        (pageSize la.albumPage).bodyViewport
                        -- currently scrolled thing is the album;
                        -- don't want to save that anywhere in the list of parents
                        la.parents
                    )
                    (Album << PageMsg)
                    (List.map Tuple.first la.parents)
                    la.flags

        LoadedList ll ->
            case ll.listPage of
                AlbumListPage alp ->
                    withHomeLink ll.home ll.flags <|
                        AlbumListPage.view
                            (AlbumListPage alp)
                            a
                            (viewList
                                albumBootstrap
                                alp.bodyViewport
                                (( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport ) :: alp.parents)
                            )
                            (\album ->
                                Album <|
                                    ViewAlbum
                                        (Thumbs
                                            { album = album
                                            , vpInfo = { bodyViewport = alp.bodyViewport, rootDivViewport = ll.rootDivViewport }
                                            , justLoadedImages = Set.empty
                                            , readyToDisplayImages = Set.empty
                                            }
                                        )
                                    <|
                                        ( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport )
                                            :: alp.parents
                            )
                            (Scroll << RawScrolledTo)
                            ll.flags


viewList : MainAlbumModel -> Viewport -> List ( AlbumList, Maybe Float ) -> AlbumList -> MainAlbumMsg
viewList oldModel viewport parents list =
    Album <|
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


withHomeLink : Maybe String -> MainAlbumFlags -> Html MainAlbumMsg -> Html MainAlbumMsg
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
