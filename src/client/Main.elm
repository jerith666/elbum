module Main exposing (AlbumBootstrap(..), AlbumBootstrapMsg(..), PostLoadNavState(..), UrlLoadState(..), decodeAlbumRequest, decodeUrlResult, findChild, findImg, flagsOf, getUrl, getUrls, gotHome, handleGetResponse, hashForAlbum, hashForList, hashFromAlbumPath, homeOf, init, justLoadedReadyToDisplayNextState, locFor, main, navForAlbum, navFrom, navToMsg, pageSize, pathsToCmd, pathsToCmdImpl, queryFor, scrollToCmd, scrollToTop, sequence, subscriptions, update, updateImageResult, urlNextState, view, viewList, withHomeLink, withPaths, withScroll, withScrollPos)

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
    = Sizing Key AlbumBootstrapFlags (Maybe (List String)) (Maybe Float)
    | LoadingHomeLink Key Viewport AlbumBootstrapFlags (Maybe (List String)) (Maybe Float)
    | Loading Key Viewport AlbumBootstrapFlags (Maybe String) (Maybe (List String)) (Maybe Float)
    | LoadError Key AlbumBootstrapFlags Http.Error
    | LoadedList Key AlbumListPage AlbumBootstrapFlags (Maybe String) (Dict String UrlLoadState) (Maybe Float) PostLoadNavState
    | LoadedAlbum Key AlbumPage (List ( AlbumList, Maybe Float )) AlbumBootstrapFlags (Maybe String) (Dict String UrlLoadState) (Maybe Float) PostLoadNavState


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
    | YesAlbum AlbumOrList
    | NoAlbum Http.Error
    | PageMsg AlbumPage.AlbumPageMsg
    | ViewList AlbumListPage (Maybe Float)
    | ViewAlbum AlbumPage (List ( AlbumList, Maybe Float ))
    | ImageLoaded String
    | ImageReadyToDisplay String
    | ImageFailed String Http.Error
    | ScheduleScroll (Maybe Float)
    | ScrolledTo Float
    | ScrollSucceeded
    | ScrollFailed String
    | Nav (List String)
    | Scroll Float
    | Sequence AlbumBootstrapMsg (List AlbumBootstrapMsg)
    | SequenceCmd (Cmd AlbumBootstrapMsg) (List (Cmd AlbumBootstrapMsg))
    | NoBootstrap


main : RouteUrlProgram AlbumBootstrapFlags AlbumBootstrap AlbumBootstrapMsg
main =
    RouteUrl.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , delta2url = locFor
        , location2messages = navToMsg
        }


init : AlbumBootstrapFlags -> Key -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
init flags key =
    ( Sizing key flags Nothing Nothing
    , Task.perform Resize getViewport
    )


update : AlbumBootstrapMsg -> AlbumBootstrap -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
update msg model =
    case Debug.log "update msg" msg of
        Resize viewport ->
            case model of
                Sizing key flags paths scroll ->
                    ( LoadingHomeLink key (Debug.log "window size set" viewport) flags paths scroll
                    , Http.send (either NoHome YesHome) <| Http.getString "home"
                    )

                LoadingHomeLink key oldSize flags paths scroll ->
                    ( LoadingHomeLink key viewport flags paths scroll
                    , Cmd.none
                    )

                Loading key oldSize flags home paths scroll ->
                    ( Loading key (Debug.log "window size updated during load" viewport) flags home paths scroll
                    , Cmd.none
                    )

                LoadError _ _ _ ->
                    ( model, Cmd.none )

                LoadedAlbum key albumPage parents flags home pendingUrls scrollPos postLoadNavState ->
                    case albumPage of
                        Thumbs album oldSize justLoadedImages readyToDisplayImages ->
                            let
                                newModel =
                                    Thumbs album (Debug.log "window size updated for thumbs" viewport) justLoadedImages readyToDisplayImages

                                urls =
                                    AlbumPage.urlsToGet newModel
                            in
                            ( LoadedAlbum key
                                newModel
                                parents
                                flags
                                home
                                (Dict.union pendingUrls <|
                                    dictWithValues urls UrlRequested
                                )
                                scrollPos
                                postLoadNavState
                            , getUrls pendingUrls urls
                            )

                        FullImage album index loaded oldSize savedScroll dragInfo ->
                            ( LoadedAlbum key (FullImage album index loaded (Debug.log "window size updated for full" viewport) savedScroll dragInfo) parents flags home pendingUrls scrollPos postLoadNavState
                            , Cmd.none
                            )

                LoadedList key (AlbumListPage albumList oldSize parentLists) flags home pendingUrls scrollPos postLoadNavState ->
                    ( LoadedList key (AlbumListPage albumList viewport parentLists) flags home pendingUrls scrollPos postLoadNavState
                    , Cmd.none
                    )

        YesHome home ->
            case model of
                LoadingHomeLink key size flags path scroll ->
                    gotHome key size flags path scroll <| Just home

                _ ->
                    ( model, Cmd.none )

        NoHome err ->
            case model of
                LoadingHomeLink key size flags path scroll ->
                    gotHome key size flags path scroll Nothing

                _ ->
                    ( model, Cmd.none )

        YesAlbum albumOrList ->
            case model of
                Loading key viewport flags home paths scroll ->
                    case albumOrList of
                        List albumList ->
                            let
                                newModel =
                                    LoadedList key (AlbumListPage albumList viewport []) flags home Dict.empty Nothing NavInactive

                                pathsThenScroll =
                                    toCmd <| sequence (pathsToCmd newModel paths) <| fromMaybe <| scrollToCmd newModel scroll
                            in
                            ( newModel
                            , pathsThenScroll
                            )

                        Leaf album ->
                            let
                                albumPage =
                                    Thumbs album viewport Set.empty Set.empty

                                urls =
                                    AlbumPage.urlsToGet albumPage

                                newModel =
                                    LoadedAlbum key albumPage [] flags home (dictWithValues urls UrlRequested) Nothing NavInactive

                                pathsThenScroll =
                                    toCmd <| sequence (pathsToCmd newModel paths) <| fromMaybe <| scrollToCmd newModel scroll
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
            ( LoadError (keyOf model) (flagsOf model) err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                LoadedAlbum key oldPage parents flags home oldPendingUrls scrollPos postLoadNavState ->
                    let
                        ( newPage, newPageCmd ) =
                            AlbumPage.update pageMsg oldPage scrollPos

                        newPendingUrls =
                            if AlbumPage.resetUrls pageMsg then
                                Dict.empty

                            else
                                oldPendingUrls

                        urls =
                            AlbumPage.urlsToGet newPage
                    in
                    ( LoadedAlbum key newPage parents flags home (Dict.union newPendingUrls <| dictWithValues urls UrlRequested) scrollPos postLoadNavState
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
                    LoadedList (keyOf model) albumListPage (flagsOf model) (homeOf model) Dict.empty Nothing NavInactive

                scrollCmd =
                    case maybeScroll of
                        Just pos ->
                            Task.attempt (\_ -> NoBootstrap) <| setViewportOf rootDivId 0 pos

                        Nothing ->
                            scrollToTop

                title =
                    case albumListPage of
                        AlbumListPage albumList _ _ ->
                            albumList.listTitle
            in
            ( newModel
            , scrollCmd
            )

        ViewAlbum albumPage parents ->
            let
                urls =
                    AlbumPage.urlsToGet albumPage

                newModel =
                    LoadedAlbum (keyOf model) albumPage parents (flagsOf model) (homeOf model) (dictWithValues urls UrlRequested) Nothing NavInactive
            in
            ( newModel
            , Cmd.batch
                [ scrollToTop
                , getUrls Dict.empty urls
                ]
            )

        ScrolledTo pos ->
            ( withScrollPos pos model, Cmd.none )

        ScheduleScroll scroll ->
            ( model
            , Maybe.withDefault Cmd.none <|
                Maybe.map
                    (\s ->
                        Task.attempt
                            (\_ -> NoBootstrap)
                        <|
                            setViewportOf rootDivId 0 <|
                                Debug.log "startup scroll to" s
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
                    ( nextModel, Debug.log ("sequence msg " ++ Debug.toString next ++ " (last) produces cmd") nextCmd )

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
                    ( rModel, toCmd <| SequenceCmd (Debug.log ("sequence msg " ++ Debug.toString next ++ " (cont'd) produces cmd") nextCmd) [ rCmds ] )

        SequenceCmd next rest ->
            let
                cmds =
                    case rest of
                        [] ->
                            Debug.log "sequenced cmd: last" next

                        r1 :: rs ->
                            Cmd.batch [ Debug.log "sequenced cmd: next" next, toCmd <| SequenceCmd r1 rs ]
            in
            ( model, cmds )

        NoBootstrap ->
            ( model, Cmd.none )


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


gotHome : Key -> Viewport -> AlbumBootstrapFlags -> Maybe (List String) -> Maybe Float -> Maybe String -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
gotHome key viewport flags paths scroll home =
    ( Loading key viewport flags home paths scroll
    , Task.attempt decodeAlbumRequest (Http.toTask (Http.get "album.json" jsonDecAlbumOrList))
    )


navToMsg : Url -> List AlbumBootstrapMsg
navToMsg loc =
    let
        parsedHash =
            Debug.log ("parsedHash from " ++ Maybe.withDefault "<no fragment>" loc.fragment) <| parseHash <| Maybe.withDefault "" loc.fragment

        parsedQuery =
            Debug.log ("parsedQuery from " ++ Maybe.withDefault "<no query>" loc.query) <| parseQuery <| Maybe.withDefault "" loc.query

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
        Sizing _ flags _ _ ->
            flags

        LoadingHomeLink _ _ flags _ _ ->
            flags

        Loading _ _ flags _ _ _ ->
            flags

        LoadError _ flags _ ->
            flags

        LoadedList _ _ flags _ _ _ _ ->
            flags

        LoadedAlbum _ _ _ flags _ _ _ _ ->
            flags


homeOf : AlbumBootstrap -> Maybe String
homeOf model =
    case model of
        Sizing _ _ _ _ ->
            Nothing

        LoadingHomeLink _ _ _ _ _ ->
            Nothing

        Loading _ _ _ home _ _ ->
            home

        LoadError _ _ _ ->
            Nothing

        LoadedList _ _ _ home _ _ _ ->
            home

        LoadedAlbum _ _ _ _ home _ _ _ ->
            home


keyOf : AlbumBootstrap -> Key
keyOf model =
    case model of
        Sizing key _ _ _ ->
            key

        LoadingHomeLink key _ _ _ _ ->
            key

        Loading key _ _ _ _ _ ->
            key

        LoadError key _ _ ->
            key

        LoadedList key _ _ _ _ _ _ ->
            key

        LoadedAlbum key _ _ _ _ _ _ _ ->
            key


withScrollPos : Float -> AlbumBootstrap -> AlbumBootstrap
withScrollPos pos model =
    case model of
        Sizing _ _ _ _ ->
            model

        LoadingHomeLink _ _ _ _ _ ->
            model

        Loading _ _ _ _ _ _ ->
            model

        LoadError _ _ _ ->
            model

        LoadedAlbum key albumPage parents flags home pendingUrls _ postLoadNavState ->
            LoadedAlbum key albumPage parents flags home pendingUrls (Just pos) postLoadNavState

        LoadedList key (AlbumListPage albumList viewport parents) flags home pendingUrls _ postLoadNavState ->
            LoadedList key (AlbumListPage albumList viewport parents) flags home pendingUrls (Just pos) postLoadNavState


withPaths : AlbumBootstrap -> List String -> AlbumBootstrap
withPaths model paths =
    case model of
        Sizing key flags _ scroll ->
            Sizing key flags (Just paths) scroll

        LoadingHomeLink key viewport flags _ scroll ->
            LoadingHomeLink key viewport flags (Just paths) scroll

        Loading key viewport flags home _ scroll ->
            Loading key viewport flags home (Just paths) scroll

        LoadError _ _ _ ->
            model

        LoadedList key albumListPage flags home pendingUrls scroll _ ->
            LoadedList key albumListPage flags home pendingUrls scroll NavInProgress

        LoadedAlbum key albumPage parents flags home pendingUrls scroll _ ->
            LoadedAlbum key albumPage parents flags home pendingUrls scroll NavInProgress


withScroll : AlbumBootstrap -> Float -> AlbumBootstrap
withScroll model scroll =
    case model of
        Sizing key flags paths _ ->
            Sizing key flags paths <| Just scroll

        LoadingHomeLink key viewport flags paths _ ->
            LoadingHomeLink key viewport flags paths <| Just scroll

        Loading key viewport flags home paths _ ->
            Loading key viewport flags home paths <| Just scroll

        LoadError _ _ _ ->
            model

        LoadedList _ _ _ _ _ _ _ ->
            model

        LoadedAlbum _ _ _ _ _ _ _ _ ->
            model


pathsToCmd : AlbumBootstrap -> Maybe (List String) -> Maybe AlbumBootstrapMsg
pathsToCmd model mPaths =
    case mPaths of
        Nothing ->
            Debug.log "pathsToCmd has no paths" Nothing

        Just paths ->
            case model of
                Sizing _ _ _ _ ->
                    Nothing

                LoadingHomeLink _ _ _ _ _ ->
                    Nothing

                Loading _ _ _ _ _ _ ->
                    Nothing

                LoadError _ _ _ ->
                    Debug.log "pathsToCmd LoadError, ignore" Nothing

                LoadedList _ (AlbumListPage albumList viewport parents) _ _ _ _ _ ->
                    --TODO maybe don't always prepend aTN here, only if at root?
                    --TODO I think it's okay to drop the scroll positions here, should only happen at initial load (?)
                    pathsToCmdImpl viewport (albumList :: List.map Tuple.first parents) paths

                LoadedAlbum _ albumPage parents _ _ _ _ _ ->
                    pathsToCmdImpl (pageSize albumPage) (List.map Tuple.first parents) paths


pathsToCmdImpl : Viewport -> List AlbumList -> List String -> Maybe AlbumBootstrapMsg
pathsToCmdImpl viewport parents paths =
    let
        mRoot =
            List.head <| List.reverse parents
    in
    case mRoot of
        Nothing ->
            Debug.log "pathsToCmdImpl has no root" Nothing

        Just root ->
            navFrom viewport root [] paths <| Just <| ViewList (AlbumListPage root viewport []) Nothing


scrollToCmd : AlbumBootstrap -> Maybe Float -> Maybe AlbumBootstrapMsg
scrollToCmd model scroll =
    let
        scrollCmd =
            Just <| ScheduleScroll scroll
    in
    case model of
        Sizing _ _ _ _ ->
            Nothing

        LoadingHomeLink _ _ _ _ _ ->
            Nothing

        Loading _ _ _ _ _ _ ->
            Nothing

        LoadError _ _ _ ->
            Debug.log "scrollToCmd LoadError, ignore" Nothing

        LoadedList _ _ _ _ _ _ _ ->
            scrollCmd

        LoadedAlbum _ _ _ _ _ _ _ _ ->
            scrollCmd


navFrom : Viewport -> AlbumList -> List AlbumList -> List String -> Maybe AlbumBootstrapMsg -> Maybe AlbumBootstrapMsg
navFrom viewport root parents paths defcmd =
    case paths of
        [] ->
            Debug.log "navFrom has no paths" defcmd

        [ "#" ] ->
            Debug.log "navFrom has only # path" defcmd

        p1 :: ps ->
            let
                mChild =
                    findChild root p1

                newParents =
                    root :: parents
            in
            case mChild of
                Nothing ->
                    Debug.log ("navFrom can't find child " ++ p1) defcmd

                Just pChild ->
                    case pChild of
                        List albumList ->
                            navFrom viewport albumList newParents ps <| Just <| ViewList (AlbumListPage albumList viewport <| List.map (\p -> ( p, Nothing )) newParents) Nothing

                        Leaf album ->
                            navForAlbum viewport album ps newParents


navForAlbum : Viewport -> Album -> List String -> List AlbumList -> Maybe AlbumBootstrapMsg
navForAlbum viewport album ps newParents =
    let
        parentsNoScroll =
            List.map (\p -> ( p, Nothing )) newParents
    in
    case ps of
        [] ->
            Just <| ViewAlbum (Thumbs album viewport Set.empty Set.empty) parentsNoScroll

        i :: _ ->
            case findImg [] album i of
                Nothing ->
                    Debug.log ("navForAlbum can't find image " ++ i) Nothing

                Just ( prevs, nAlbum ) ->
                    let
                        ( w, h ) =
                            fitImage nAlbum.imageFirst.srcSetFirst (floor viewport.viewport.width) (floor viewport.viewport.height)

                        ( progModel, progCmd ) =
                            progInit viewport nAlbum.imageFirst w h
                    in
                    Just <|
                        Sequence
                            (ViewAlbum (FullImage prevs nAlbum progModel viewport Nothing Nothing) parentsNoScroll)
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
                LoadedList _ oAlbumListPage _ _ _ _ _ ->
                    case newModel of
                        LoadedList _ nAlbumListPage _ _ _ _ _ ->
                            case oAlbumListPage == nAlbumListPage of
                                True ->
                                    Debug.log "locFor LoadedList same" ModifyEntry

                                False ->
                                    Debug.log "locFor LoadedList dift" NewEntry

                        _ ->
                            Debug.log "locFor LoadedList -> something else" NewEntry

                LoadedAlbum _ oAlbumPage oParents _ _ _ _ _ ->
                    case newModel of
                        LoadedAlbum _ nAlbumPage nParents _ _ _ _ _ ->
                            case oAlbumPage == nAlbumPage && oParents == nParents of
                                True ->
                                    Debug.log "locFor LoadedAlbum same" ModifyEntry

                                False ->
                                    Debug.log "locFor LoadedAlbum dift" NewEntry

                        _ ->
                            Debug.log "locFor LoadedAlbum -> something else" NewEntry

                _ ->
                    Debug.log "locFor something els -> *" NewEntry

        checkNavState state nav =
            case Debug.log "checkNavState" state of
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
    Debug.log "locFor" <|
        case model of
            LoadedAlbum key albumPage parents _ _ _ _ postLoadNavState ->
                checkNavState postLoadNavState <|
                    Just <|
                        newQorF { entry = entry, key = key }
                            model
                        <|
                            hashForAlbum model albumPage <|
                                List.map Tuple.first parents

            LoadedList key albumListPage _ _ _ _ postLoadNavState ->
                checkNavState postLoadNavState <|
                    Just <|
                        newQorF { entry = entry, key = key }
                            model
                        <|
                            hashForList model albumListPage

            _ ->
                Nothing


queryFor : AlbumBootstrap -> String
queryFor model =
    let
        queryForPos pos =
            Maybe.withDefault "" <| Maybe.map (\p -> "s=" ++ String.fromFloat p) pos
    in
    case model of
        Sizing _ _ _ _ ->
            ""

        LoadingHomeLink _ _ _ _ _ ->
            ""

        Loading _ _ _ _ _ _ ->
            ""

        LoadError _ _ _ ->
            ""

        LoadedAlbum _ _ _ _ _ _ pos _ ->
            queryForPos pos

        LoadedList _ _ _ _ _ pos _ ->
            queryForPos pos


hashForList : AlbumBootstrap -> AlbumListPage -> String
hashForList model (AlbumListPage albumList _ parents) =
    if List.isEmpty parents then
        hashFromAlbumPath model [ "" ] []

    else
        hashFromAlbumPath model [ albumList.listTitle ] <| List.map Tuple.first parents


hashForAlbum : AlbumBootstrap -> AlbumPage -> List AlbumList -> String
hashForAlbum model albumPage parents =
    let
        titles =
            case albumPage of
                Thumbs album _ _ _ ->
                    [ album.title ]

                FullImage _ album _ _ _ _ ->
                    [ album.title, album.imageFirst.altText ]
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
        LoadedAlbum key albumPage parents flags home pendingUrls scrollPos postLoadNavState ->
            case albumPage of
                Thumbs album viewport justLoadedImages readyToDisplayImages ->
                    let
                        newModel =
                            justLoadedReadyToDisplayNextState album viewport justLoadedImages readyToDisplayImages url result

                        urls =
                            AlbumPage.urlsToGet newModel
                    in
                    ( LoadedAlbum key
                        newModel
                        parents
                        flags
                        home
                        (Dict.union (Dict.fromList [ ( url, result ) ]) <|
                            Dict.union pendingUrls <|
                                dictWithValues urls UrlRequested
                        )
                        scrollPos
                        postLoadNavState
                    , Cmd.batch
                        [ getUrls pendingUrls urls
                        , urlNextState url result
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


justLoadedReadyToDisplayNextState : Album -> Viewport -> Set String -> Set String -> String -> UrlLoadState -> AlbumPage
justLoadedReadyToDisplayNextState album viewport justLoadedImages readyToDisplayImages url result =
    case result of
        JustCompleted ->
            Thumbs album viewport (Set.insert url justLoadedImages) readyToDisplayImages

        ReadyToDisplay ->
            Thumbs album viewport (Set.remove url justLoadedImages) <| Set.insert url readyToDisplayImages

        _ ->
            Thumbs album viewport justLoadedImages readyToDisplayImages


urlNextState : String -> UrlLoadState -> Cmd AlbumBootstrapMsg
urlNextState url result =
    case result of
        JustCompleted ->
            Delay.after 100 Millisecond <| ImageReadyToDisplay url

        _ ->
            Cmd.none


decodeAlbumRequest : Result Http.Error AlbumOrList -> AlbumBootstrapMsg
decodeAlbumRequest r =
    case r of
        Ok a ->
            YesAlbum a

        Err e ->
            NoAlbum e


getUrls : Dict String UrlLoadState -> Set String -> Cmd AlbumBootstrapMsg
getUrls existingUrls newUrls =
    Cmd.batch <| List.map getUrl <| Set.toList <| Set.diff newUrls <| Set.fromList <| keys existingUrls


getUrl : String -> Cmd AlbumBootstrapMsg
getUrl url =
    Task.attempt (decodeUrlResult url)
        (Http.toTask
            (Http.request
                { method = "GET"
                , headers = []
                , url = encodePath url
                , body = emptyBody
                , expect = expectStringResponse <| handleGetResponse url
                , timeout = Nothing
                , withCredentials = False
                }
            )
        )


handleGetResponse : String -> Response String -> Result String String
handleGetResponse url r =
    case r.status.code of
        200 ->
            Ok url

        _ ->
            Err <| "err " ++ r.status.message


decodeUrlResult : String -> Result Http.Error String -> AlbumBootstrapMsg
decodeUrlResult origUrl result =
    case result of
        Ok url ->
            ImageLoaded url

        Err e ->
            ImageFailed origUrl e


subscriptions : AlbumBootstrap -> Sub AlbumBootstrapMsg
subscriptions model =
    case model of
        LoadedAlbum _ albumPage parents _ _ _ _ _ ->
            let
                showParent =
                    case parents of
                        [] ->
                            NoBootstrap

                        ( parent, scroll ) :: grandParents ->
                            ViewList (AlbumListPage parent (pageSize albumPage) grandParents) scroll
            in
            Sub.batch
                [ AlbumPage.subscriptions albumPage PageMsg showParent
                , onResize <| newSize <| pageSize albumPage
                ]

        LoadedList _ (AlbumListPage albumList viewport parents) _ _ _ _ _ ->
            case parents of
                [] ->
                    onResize <| newSize viewport

                ( parent, scroll ) :: grandParents ->
                    let
                        upParent =
                            onEscape
                                (ViewList (AlbumListPage parent viewport grandParents) scroll)
                                NoBootstrap
                    in
                    Sub.batch [ upParent, onResize <| newSize viewport ]

        LoadingHomeLink _ viewport _ _ _ ->
            onResize <| newSize viewport

        Loading _ viewport _ _ _ _ ->
            onResize <| newSize viewport

        LoadError _ _ _ ->
            Sub.none

        Sizing _ _ _ _ ->
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


pageSize : AlbumPage -> Viewport
pageSize albumPage =
    case albumPage of
        Thumbs _ viewport _ _ ->
            viewport

        FullImage _ _ _ viewport _ _ ->
            viewport


onUrlRequest : UrlRequest -> AlbumBootstrapMsg
onUrlRequest request =
    let
        durl =
            case request of
                Internal url ->
                    toString url

                External url ->
                    url
    in
    Debug.log ("no-op onUrlRequest " ++ durl) <| NoBootstrap


view : AlbumBootstrap -> Document AlbumBootstrapMsg
view albumBootstrap =
    let
        title =
            case albumBootstrap of
                Sizing _ _ _ _ ->
                    "Album Starting"

                LoadingHomeLink _ _ _ _ _ ->
                    "Home Loading ..."

                Loading _ _ _ _ _ _ ->
                    "Album Loading ..."

                LoadError _ _ _ ->
                    "Error Loading Album"

                LoadedList _ (AlbumListPage albumList _ _) _ _ _ _ _ ->
                    albumList.listTitle

                LoadedAlbum _ albumPage _ _ _ _ _ _ ->
                    titleOf albumPage
    in
    { title = title
    , body = [ viewImpl albumBootstrap |> toUnstyled ]
    }


viewImpl : AlbumBootstrap -> Html AlbumBootstrapMsg
viewImpl albumBootstrap =
    case albumBootstrap of
        Sizing _ _ _ _ ->
            text "Album Starting"

        LoadingHomeLink _ _ _ _ _ ->
            text "Home Loading ..."

        Loading _ _ _ _ _ _ ->
            text "Album Loading ..."

        LoadError _ _ e ->
            let
                eStr =
                    case e of
                        BadUrl s ->
                            "bad url: " ++ s

                        Timeout ->
                            "timeout"

                        NetworkError ->
                            "network error"

                        BadStatus response ->
                            "bad status: " ++ response.status.message ++ ": " ++ response.body

                        BadPayload s response ->
                            "bad payload: " ++ s
            in
            text ("Error Loading Album: " ++ eStr)

        LoadedAlbum _ albumPage parents flags home pendingUrls scrollPos postLoadNavState ->
            withHomeLink home flags <|
                AlbumPage.view
                    albumPage
                    ScrolledTo
                    (viewList
                        albumBootstrap
                        (pageSize albumPage)
                        -- currently scrolled thing is the album;
                        -- don't want to save that anywhere in the list of parents
                        parents
                    )
                    PageMsg
                    (List.map Tuple.first parents)
                    flags

        LoadedList _ (AlbumListPage albumList viewport parents) flags home pendingUrls scrollPos postLoadNavState ->
            withHomeLink home flags <|
                AlbumListPage.view
                    (AlbumListPage
                        albumList
                        viewport
                        parents
                    )
                    (viewList
                        albumBootstrap
                        viewport
                        (( albumList, scrollPos ) :: parents)
                    )
                    (\album ->
                        ViewAlbum
                            (Thumbs album viewport Set.empty Set.empty)
                        <|
                            ( albumList, scrollPos )
                                :: parents
                    )
                    ScrolledTo
                    flags


viewList : AlbumBootstrap -> Viewport -> List ( AlbumList, Maybe Float ) -> AlbumList -> AlbumBootstrapMsg
viewList oldModel viewport parents list =
    ViewList
        (AlbumListPage list viewport <|
            dropThroughPred
                (\( p, _ ) -> p == list)
                parents
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
