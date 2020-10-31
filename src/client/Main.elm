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
    = AwaitingBaseUrl
        { key : Key
        , flags : MainAlbumFlags
        , albumPathsAfterLoad : Maybe (List String)
        }
    | Sizing
        { key : Key
        , baseUrl : Url
        , flags : MainAlbumFlags
        , albumPathsAfterLoad : Maybe (List String)
        }
    | LoadingHomeLink
        { key : Key
        , baseUrl : Url
        , bodyViewport : Viewport
        , flags : MainAlbumFlags
        , albumPathsAfterLoad : Maybe (List String)
        }
    | Loading
        { key : Key
        , baseUrl : Url
        , bodyViewport : Viewport
        , progress : Maybe Progress
        , flags : MainAlbumFlags
        , home : Maybe String
        , albumPathsAfterLoad : Maybe (List String)
        }
    | LoadError
        { key : Key
        , flags : MainAlbumFlags
        , error : Http.Error
        }
    | LoadedList
        { key : Key
        , baseUrl : Url
        , listPage : AlbumListPage
        , flags : MainAlbumFlags
        , home : Maybe String
        , rootDivViewport : Maybe Viewport
        , navState : PostLoadNavState
        }
    | LoadedAlbum
        { key : Key
        , baseUrl : Url
        , albumPage : AlbumPage
        , parents : List ( AlbumList, Maybe Float )
        , flags : MainAlbumFlags
        , home : Maybe String
        , rootDivViewport : Maybe Viewport
        , navState : PostLoadNavState
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
    | General GeneralMsg


type MetaMsg
    = Sequence MainAlbumMsg (List MainAlbumMsg)
    | SequenceCmd (Cmd MainAlbumMsg) (List (Cmd MainAlbumMsg))
    | NoBootstrap


type BootstrapMsg
    = GotBaseUrl Url
    | YesHome String
    | NoHome Http.Error
    | LoadAlbumProgress Progress
    | YesAlbum AlbumOrList
    | NoAlbum Http.Error


type AlbumMsg
    = SetAlbumPathFromUrl (List String)
    | PageMsg AlbumPage.AlbumPageMsg
    | ViewList AlbumListPage (Maybe Float)
    | ViewAlbum AlbumPage (List ( AlbumList, Maybe Float ))
    | NavCompletedLocally


type GeneralMsg
    = Resize Viewport
    | ScrolledTo Viewport
    | InternalUrlClicked Url
    | ExternalLinkClicked String


main : RouteUrlProgram MainAlbumFlags MainAlbumModel MainAlbumMsg
main =
    RouteUrl.anchorManagedProgram
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onExternalUrlRequest = General << ExternalLinkClicked
        , delta2url = locFor
        , location2messages = \u -> [ General <| InternalUrlClicked u ]
        , makeAnchor = makeAnchor
        }


makeAnchor : String -> Maybe MainAlbumMsg -> List (Attribute MainAlbumMsg) -> List (Html MainAlbumMsg) -> Html MainAlbumMsg
makeAnchor url onClickMsg attrs =
    a <|
        href url
            :: (case onClickMsg of
                    Nothing ->
                        attrs

                    Just m ->
                        Html.Styled.Events.onClick m :: attrs
               )


init : MainAlbumFlags -> Key -> ( MainAlbumModel, Cmd MainAlbumMsg )
init flags key =
    ( AwaitingBaseUrl { key = key, flags = flags, albumPathsAfterLoad = Nothing }
    , Cmd.none
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

        General generalMsg ->
            updateGeneral generalMsg model


updateGeneral : GeneralMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
updateGeneral generalMsg model =
    case generalMsg of
        Resize viewport ->
            case model of
                AwaitingBaseUrl _ ->
                    ( model, Cmd.none )

                Sizing sz ->
                    ( LoadingHomeLink
                        { key = sz.key
                        , baseUrl = sz.baseUrl
                        , bodyViewport = log "window size set" viewport
                        , flags = sz.flags
                        , albumPathsAfterLoad = sz.albumPathsAfterLoad
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
                            in
                            ( LoadedAlbum
                                { la | albumPage = newModel }
                            , Cmd.none
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

        ScrolledTo viewport ->
            ( withScrollPos (log "ScrolledTo: " viewport) model, Cmd.none )

        InternalUrlClicked url ->
            ( model, navToMsg model url )

        ExternalLinkClicked url ->
            --home link should be only external link in our app
            ( model, load <| log "loading external url, assumed to be home" url )


updateBootstrap : BootstrapMsg -> MainAlbumModel -> ( MainAlbumModel, Cmd MainAlbumMsg )
updateBootstrap bootstrapMsg model =
    case bootstrapMsg of
        GotBaseUrl url ->
            case model of
                AwaitingBaseUrl abu ->
                    ( Sizing { key = abu.key, baseUrl = url, flags = abu.flags, albumPathsAfterLoad = abu.albumPathsAfterLoad }
                    , Task.perform (General << Resize) getViewport
                    )

                _ ->
                    ( model, Cmd.none )

        YesHome home ->
            case model of
                LoadingHomeLink lh ->
                    gotHome lh <| Just <| String.trim home

                _ ->
                    ( model, Cmd.none )

        NoHome _ ->
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
                                        , baseUrl = ld.baseUrl
                                        , listPage =
                                            AlbumListPage
                                                { albumList = albumList
                                                , bodyViewport = ld.bodyViewport
                                                , parents = []
                                                }
                                        , flags = ld.flags
                                        , home = ld.home
                                        , rootDivViewport = Nothing
                                        , navState = NavInactive
                                        }
                            in
                            ( newModel
                            , Maybe.withDefault Cmd.none <|
                                Maybe.map toCmd <|
                                    pathsToCmd newModel ld.albumPathsAfterLoad
                            )

                        Leaf album ->
                            let
                                ( albumPageModel, albumPageCmd ) =
                                    initThumbs album ld.bodyViewport ld.baseUrl

                                newModel =
                                    LoadedAlbum
                                        { key = ld.key
                                        , baseUrl = ld.baseUrl
                                        , albumPage = albumPageModel
                                        , parents = []
                                        , flags = ld.flags
                                        , home = ld.home
                                        , rootDivViewport = Nothing
                                        , navState = NavInactive
                                        }
                            in
                            ( newModel
                            , Cmd.batch
                                [ Cmd.map (Album << PageMsg) albumPageCmd
                                , Maybe.withDefault Cmd.none <|
                                    Maybe.map toCmd <|
                                        pathsToCmd newModel ld.albumPathsAfterLoad
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
                    in
                    ( LoadedAlbum
                        { la | albumPage = newPage }
                    , Cmd.map (Album << PageMsg) newPageCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ViewList albumListPage maybeScroll ->
            case baseUrlOf model of
                Just baseUrl ->
                    let
                        newModel =
                            LoadedList
                                { key = keyOf model
                                , baseUrl = baseUrl
                                , listPage = albumListPage
                                , flags = flagsOf model
                                , home = homeOf model
                                , rootDivViewport = Nothing
                                , navState = NavInactive
                                }

                        scrollCmd =
                            case maybeScroll of
                                Just pos ->
                                    Task.attempt (always NoBootstrap) <| setViewportOf rootDivId 0 pos

                                Nothing ->
                                    scrollToTop NoBootstrap <| always NoBootstrap
                    in
                    ( newModel
                    , Cmd.map Meta scrollCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ViewAlbum albumPage parents ->
            case baseUrlOf model of
                Just baseUrl ->
                    let
                        newModel =
                            LoadedAlbum
                                { key = keyOf model
                                , baseUrl = baseUrl
                                , albumPage = albumPage
                                , parents = parents
                                , flags = flagsOf model
                                , home = homeOf model
                                , rootDivViewport = Nothing
                                , navState = NavInactive
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
                        , getUrls Dict.empty urls --TODO need something like 'AlbumPage.cmdsFor newModel'
                        , Cmd.map Album getImgPos
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        SetAlbumPathFromUrl paths ->
            -- as with SetScrollFromUrl, 2 cases: early loading, save for later; and already loaded, apply now
            -- but no hacky delay here
            ( withAlbumPathsAfterLoad model paths
            , toCmd <| Maybe.withDefault (Meta NoBootstrap) <| pathsToCmd model <| Just paths
            )

        NavCompletedLocally ->
            ( withNavComplete model, Cmd.none )


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
        , baseUrl = lh.baseUrl
        , bodyViewport = lh.bodyViewport
        , progress = Nothing
        , flags = lh.flags
        , home = home
        , albumPathsAfterLoad = lh.albumPathsAfterLoad
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


navToMsg : MainAlbumModel -> Url -> Cmd MainAlbumMsg
navToMsg model loc =
    case model of
        AwaitingBaseUrl _ ->
            Task.perform identity <| Task.succeed <| Bootstrap <| GotBaseUrl loc

        _ ->
            --home link might count as internal if it's on the same domain
            let
                hUrl =
                    Maybe.andThen Url.fromString <| homeOf model

                locIsHome =
                    Maybe.withDefault False <| Maybe.map (\h -> h == loc) <| hUrl
            in
            case locIsHome of
                True ->
                    load <| toString <| log "loading internal home url" loc

                False ->
                    navToMsgInternal <| log "navToMsgInternal for non-home internal url" loc


navToMsgInternal : Url -> Cmd MainAlbumMsg
navToMsgInternal loc =
    let
        parsedHash =
            log ("parsedHash from " ++ Maybe.withDefault "<no fragment>" loc.fragment) <| parseHash <| Maybe.withDefault "" loc.fragment

        hashMsgs =
            case parsedHash of
                Err _ ->
                    []

                Ok paths ->
                    [ Album <| SetAlbumPathFromUrl paths ]
    in
    case hashMsgs of
        [] ->
            Cmd.none

        [ c ] ->
            Task.perform identity <| Task.succeed c

        c1 :: cs ->
            Task.perform identity <| Task.succeed <| Meta <| Sequence c1 cs


flagsOf : MainAlbumModel -> MainAlbumFlags
flagsOf model =
    case model of
        AwaitingBaseUrl abu ->
            abu.flags

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
        AwaitingBaseUrl _ ->
            Nothing

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
        AwaitingBaseUrl abu ->
            abu.key

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


baseUrlOf : MainAlbumModel -> Maybe Url
baseUrlOf model =
    case model of
        AwaitingBaseUrl _ ->
            Nothing

        Sizing sz ->
            Just sz.baseUrl

        LoadingHomeLink lhl ->
            Just lhl.baseUrl

        Loading l ->
            Just l.baseUrl

        LoadError le ->
            Nothing

        LoadedList ll ->
            Just ll.baseUrl

        LoadedAlbum la ->
            Just la.baseUrl


withScrollPos : Viewport -> MainAlbumModel -> MainAlbumModel
withScrollPos rootDivViewport model =
    case model of
        AwaitingBaseUrl _ ->
            model

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
        AwaitingBaseUrl abu ->
            AwaitingBaseUrl { abu | albumPathsAfterLoad = Just albumPathsAfterLoad }

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


withNavComplete : MainAlbumModel -> MainAlbumModel
withNavComplete model =
    case model of
        AwaitingBaseUrl _ ->
            model

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
                AwaitingBaseUrl _ ->
                    Nothing

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
                            log "pathsToCmd for LoadedList" <|
                                pathsToCmdImpl ll.baseUrl
                                    model
                                    { bodyViewport = alp.bodyViewport, rootDivViewport = ll.rootDivViewport }
                                    (alp.albumList :: List.map Tuple.first alp.parents)
                                    paths

                LoadedAlbum la ->
                    log "pathsToCmd for LoadedAlbum" <| pathsToCmdImpl la.baseUrl model (pageSize la.albumPage) (List.map Tuple.first la.parents) paths


pathsToCmdImpl : Url -> MainAlbumModel -> ViewportInfo -> List AlbumList -> List String -> Maybe MainAlbumMsg
pathsToCmdImpl baseUrl model viewport parents paths =
    let
        mRoot =
            List.head <| List.reverse parents
    in
    case mRoot of
        Nothing ->
            log "pathsToCmdImpl has no root" Nothing

        Just root ->
            let
                modelParents =
                    case model of
                        LoadedList ll ->
                            case ll.listPage of
                                AlbumListPage alp ->
                                    alp.parents

                        LoadedAlbum la ->
                            la.parents

                        _ ->
                            []

                fallbackScroll =
                    List.reverse modelParents |> List.head |> Maybe.andThen Tuple.second

                fallbackMsg =
                    Album <|
                        ViewList
                            (AlbumListPage { albumList = root, bodyViewport = viewport.bodyViewport, parents = [] })
                            fallbackScroll
            in
            Just <| navFrom baseUrl model viewport root [] (log "pathsToCmdImpl passing paths to navFrom" paths) fallbackMsg


navFrom : Url -> MainAlbumModel -> ViewportInfo -> AlbumList -> List AlbumList -> List String -> MainAlbumMsg -> MainAlbumMsg
navFrom baseUrl model viewport root parents paths defMsg =
    case paths of
        [] ->
            log "navFrom has no paths" defMsg

        [ "#" ] ->
            log "navFrom has only # path" defMsg

        p1 :: ps ->
            let
                mChild =
                    findChild root p1

                newParents =
                    root :: parents
            in
            log ("navFrom first path " ++ p1) <|
                case mChild of
                    Nothing ->
                        log ("navFrom can't find child " ++ p1) defMsg

                    Just pChild ->
                        case pChild of
                            List albumList ->
                                let
                                    -- a generic message that will navigate to the album we found
                                    -- no matter where we currently are
                                    makeViewList bodyViewport scroll parentss =
                                        Album <|
                                            ViewList
                                                (AlbumListPage
                                                    { albumList = albumList
                                                    , bodyViewport = bodyViewport
                                                    , parents = parentss
                                                    }
                                                )
                                                scroll

                                    thisAlbumMsg =
                                        makeViewList
                                            viewport.bodyViewport
                                            Nothing
                                        <|
                                            List.map (\p -> ( p, Nothing )) newParents
                                in
                                case ps of
                                    [] ->
                                        -- check if we're at a parent or child of the list identified by
                                        -- the navigation data.  if so, use a more targeted message
                                        -- so that scroll position etc. can be preserved or restored (respectively)
                                        case model of
                                            LoadedList ll ->
                                                case ll.listPage of
                                                    AlbumListPage alp ->
                                                        let
                                                            destIsChild =
                                                                log "destIsChild" <|
                                                                    List.member
                                                                        (List albumList)
                                                                        (alp.albumList.childFirst :: alp.albumList.childRest)

                                                            destParent =
                                                                log "destParent" <|
                                                                    List.head <|
                                                                        List.filter
                                                                            (\( list, _ ) -> list == albumList)
                                                                            alp.parents

                                                            localNavWithScroll scroll =
                                                                Meta <|
                                                                    Sequence
                                                                        (makeViewList
                                                                            alp.bodyViewport
                                                                            scroll
                                                                         <|
                                                                            ( alp.albumList
                                                                            , Maybe.map scrollPosOf ll.rootDivViewport
                                                                            )
                                                                                :: alp.parents
                                                                        )
                                                                        [ Album NavCompletedLocally ]
                                                        in
                                                        case destIsChild of
                                                            True ->
                                                                localNavWithScroll Nothing

                                                            False ->
                                                                case destParent of
                                                                    Just ( _, scroll ) ->
                                                                        localNavWithScroll scroll

                                                                    Nothing ->
                                                                        thisAlbumMsg

                                            LoadedAlbum la ->
                                                let
                                                    destParent =
                                                        log "LoadedAlbum.destParent" <|
                                                            List.head <|
                                                                List.filter
                                                                    (\( list, _ ) -> list == albumList)
                                                                    la.parents
                                                in
                                                case destParent of
                                                    Just ( _, scroll ) ->
                                                        makeViewList (pageSize la.albumPage).bodyViewport scroll <|
                                                            dropThroughPred
                                                                (\( p, _ ) -> p == albumList)
                                                                la.parents

                                                    Nothing ->
                                                        thisAlbumMsg

                                            _ ->
                                                thisAlbumMsg

                                    _ ->
                                        log "navFrom recursive call" <|
                                            navFrom baseUrl model viewport albumList newParents ps thisAlbumMsg

                            Leaf album ->
                                Maybe.withDefault defMsg <|
                                    log "navFrom calls navForAlbum" <|
                                        navForAlbum baseUrl model viewport album ps newParents


navForAlbum : Url -> MainAlbumModel -> ViewportInfo -> Album -> List String -> List AlbumList -> Maybe MainAlbumMsg
navForAlbum baseUrl model vpInfo album ps newParents =
    let
        parentsNoScroll =
            List.map (\p -> ( p, Nothing )) newParents
    in
    case log "navForAlbum ps" ps of
        [] ->
            -- no more paths remain, so create a message that will
            -- take us to the thumbnails page for this album
            let
                ( x, _ ) =
                    initThumbsFullVp album vpInfo baseUrl

                makeViewAlbumThumbsMsg parents =
                    Just <|
                        Album <|
                            ViewAlbum
                                x
                                parents

                nonLocalMsg =
                    makeViewAlbumThumbsMsg parentsNoScroll
            in
            -- see if we are at the AlbumList currently containing the target Album
            -- or on a FullImagePage contained in the target Album
            -- if so, create a message that saves or restores (respectively)
            -- the current scroll position of that AlbumList or Album(Thumbs)
            case model of
                LoadedList ll ->
                    case ll.listPage of
                        AlbumListPage alp ->
                            case List.member (Leaf album) <| alp.albumList.childFirst :: alp.albumList.childRest of
                                True ->
                                    makeViewAlbumThumbsMsg <|
                                        ( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport )
                                            :: alp.parents

                                False ->
                                    nonLocalMsg

                LoadedAlbum la ->
                    case la.albumPage of
                        Thumbs _ ->
                            nonLocalMsg

                        FullImage fi ->
                            case album == baseAlbumOf (FullImage fi) of
                                True ->
                                    Just <|
                                        Meta <|
                                            Sequence
                                                (Album <| PageMsg BackToThumbs)
                                                [ Album NavCompletedLocally ]

                                False ->
                                    nonLocalMsg

                _ ->
                    nonLocalMsg

        i :: _ ->
            -- another path element remains; see if it's the name of
            -- an image in this album
            case findImg [] album i of
                Nothing ->
                    log ("navForAlbum can't find image " ++ i) Nothing

                Just ( prevs, nAlbum ) ->
                    let
                        --prepare data we need to initialize progressive
                        --loading of the full-size image we've found
                        ( w, h ) =
                            fitImage
                                nAlbum.imageFirst.srcSetFirst
                                (floor vpInfo.bodyViewport.viewport.width)
                                (floor vpInfo.bodyViewport.viewport.height)

                        ( progModel, progMsg ) =
                            progInit vpInfo.bodyViewport nAlbum.imageFirst w h

                        -- prepare the default message(s) to view the full-size image
                        -- followed by the initial message for its progressively loading
                        nonLocalMsg =
                            Just <|
                                Meta <|
                                    Sequence
                                        (Album <|
                                            ViewAlbum
                                                (FullImage
                                                    { baseUrl = baseUrl
                                                    , prevImgs = prevs
                                                    , album = nAlbum
                                                    , progModel = progModel
                                                    , vpInfo = vpInfo
                                                    , scroll = Nothing
                                                    , touchState = TU.init
                                                    , imgPosition = Nothing
                                                    , imageLoader = imageLoader
                                                    }
                                                )
                                                parentsNoScroll
                                        )
                                    <|
                                        fromMaybe <|
                                            Maybe.map (Album << PageMsg << FullMsg) progMsg
                    in
                    -- now, see if we can create a more specific message, depending on whether we're
                    -- currently viewing the thumbnails or a full size image
                    case model of
                        LoadedAlbum la ->
                            case la.albumPage of
                                Thumbs t ->
                                    case t.album == album of
                                        True ->
                                            -- we're handling an internal link from a thumbs page to a full size image,
                                            -- so create a 'View' message.  this enables us to save the scroll position
                                            -- for the outer album.
                                            log "navForAlbum t.album == album" <|
                                                Just <|
                                                    Meta <|
                                                        Sequence
                                                            (Album <| PageMsg <| View prevs nAlbum.imageFirst nAlbum.imageRest)
                                                            [ Album NavCompletedLocally ]

                                        False ->
                                            log "navForAlbum t.album != album" nonLocalMsg

                                FullImage fi ->
                                    let
                                        baseAlbum =
                                            baseAlbumOf <| FullImage fi
                                    in
                                    case baseAlbum == album of
                                        True ->
                                            -- we're handling a link from one full image to another within the same album
                                            -- that almost certainly means it's a link to the previous or next image
                                            -- confirm that and if so, use a targeted message so that scroll positions
                                            -- of all parents are preserved
                                            let
                                                matchImgMsg possibleMatchingImage msgOnMatch =
                                                    case possibleMatchingImage of
                                                        Nothing ->
                                                            Nothing

                                                        Just matchingImage ->
                                                            case nAlbum.imageFirst == matchingImage of
                                                                True ->
                                                                    Just <|
                                                                        Meta <|
                                                                            Sequence (Album <| PageMsg <| msgOnMatch)
                                                                                [ Album NavCompletedLocally ]

                                                                False ->
                                                                    Nothing

                                                nextMsg =
                                                    matchImgMsg (List.head fi.album.imageRest) Next

                                                prevMsg =
                                                    matchImgMsg (List.head (List.reverse fi.prevImgs)) Prev
                                            in
                                            case nextMsg of
                                                Just next ->
                                                    Just <| log "navForAlbum FullImage next" next

                                                Nothing ->
                                                    case prevMsg of
                                                        Just prev ->
                                                            Just <| log "navForAlbum FullImage prev" prev

                                                        Nothing ->
                                                            log "navForAlbum FullImage not prev or next" nonLocalMsg

                                        False ->
                                            log "navForAlbum fi.album != album" nonLocalMsg

                        _ ->
                            nonLocalMsg


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
    in
    log "locFor" <|
        case newModel of
            LoadedAlbum la ->
                checkNavState la.navState <|
                    Just <|
                        NewFragment entry <|
                            hashForAlbum la.albumPage <|
                                List.map Tuple.first la.parents

            LoadedList ll ->
                checkNavState ll.navState <|
                    Just <|
                        NewFragment entry <|
                            hashForList ll.listPage

            _ ->
                Nothing


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

        AwaitingBaseUrl _ ->
            Sub.none

        Sizing _ ->
            Sub.none


newSize : Viewport -> Int -> Int -> MainAlbumMsg
newSize v x y =
    General <| Resize <| viewportWithNewSize v x y


view : MainAlbumModel -> AnchorFunction MainAlbumMsg -> Document MainAlbumMsg
view albumBootstrap a =
    let
        title =
            case albumBootstrap of
                AwaitingBaseUrl _ ->
                    "Album Starting"

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


viewImpl : MainAlbumModel -> AnchorFunction MainAlbumMsg -> Html MainAlbumMsg
viewImpl albumBootstrap a =
    case albumBootstrap of
        AwaitingBaseUrl _ ->
            text "Album Starting"

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
                    (General << ScrolledTo)
                    (viewList
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
                                alp.bodyViewport
                                (( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport ) :: alp.parents)
                            )
                            (\album ->
                                Album <|
                                    ViewAlbum
                                        (Thumbs
                                            { album = album
                                            , vpInfo = { bodyViewport = alp.bodyViewport, rootDivViewport = ll.rootDivViewport }
                                            , baseUrl = ll.baseUrl
                                            , imageLoader = imageLoader
                                            }
                                        )
                                    <|
                                        ( alp.albumList, Maybe.map scrollPosOf ll.rootDivViewport )
                                            :: alp.parents
                            )
                            (General << ScrolledTo)
                            ll.flags


viewList : Viewport -> List ( AlbumList, Maybe Float ) -> AlbumList -> MainAlbumMsg
viewList viewport parents list =
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
