module Main exposing (..)

import Album exposing (..)
import AlbumListPage exposing (..)
import AlbumPage exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Delay exposing (..)
import Dict exposing (..)
import Dom exposing (..)
import Dom.Scroll exposing (..)
import FullImagePage exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http exposing (..)
import KeyboardUtils exposing (onEscape)
import ListUtils exposing (..)
import LocationUtils exposing (..)
import Navigation exposing (..)
import ResultUtils exposing (..)
import RouteUrl exposing (..)
import Set exposing (..)
import Task exposing (..)
import Time exposing (..)
import WinSize exposing (..)
import Window exposing (..)


type AlbumBootstrap
    = Sizing AlbumBootstrapFlags (Maybe (List String))
    | LoadingHomeLink WinSize AlbumBootstrapFlags (Maybe (List String))
    | Loading WinSize AlbumBootstrapFlags (Maybe String) (Maybe (List String))
    | LoadError AlbumBootstrapFlags Http.Error
    | LoadedList AlbumListPage AlbumBootstrapFlags (Maybe String) (Dict String UrlLoadState)
    | LoadedAlbum AlbumPage (List AlbumList) AlbumBootstrapFlags (Maybe String) (Dict String UrlLoadState)


type UrlLoadState
    = UrlRequested
      --| Partial Int
    | JustCompleted
    | ReadyToDisplay
    | Failed Http.Error


type AlbumBootstrapMsg
    = Resize Size
    | YesHome String
    | NoHome Http.Error
    | YesAlbum AlbumOrList
    | NoAlbum Http.Error
    | PageMsg AlbumPage.AlbumPageMsg
    | ViewList AlbumListPage
    | ViewAlbum AlbumPage (List AlbumList)
    | ImageLoaded String
    | ImageReadyToDisplay String
    | ImageFailed String Http.Error
    | ScrollSucceeded
    | ScrollFailed Id
    | Nav (List String)
    | NoBootstrap


main : RouteUrlProgram AlbumBootstrapFlags AlbumBootstrap AlbumBootstrapMsg
main =
    RouteUrl.programWithFlags
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        , delta2url = \_ -> locFor
        , location2messages = navToMsg
        }


init : AlbumBootstrapFlags -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
init flags =
    ( Sizing flags Nothing
    , Task.perform Resize Window.size
    )


update : AlbumBootstrapMsg -> AlbumBootstrap -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
update msg model =
    case msg of
        Resize size ->
            case model of
                Sizing flags paths ->
                    ( LoadingHomeLink (Debug.log "window size set" size) flags paths
                    , Http.send (either NoHome YesHome) <| Http.getString "home"
                    )

                LoadingHomeLink oldSize flags paths ->
                    ( LoadingHomeLink size flags paths
                    , Cmd.none
                    )

                Loading oldSize flags home paths ->
                    ( Loading (Debug.log "window size updated during load" size) flags home paths
                    , Cmd.none
                    )

                LoadError _ _ ->
                    ( model, Cmd.none )

                LoadedAlbum albumPage parents flags home pendingUrls ->
                    case albumPage of
                        Thumbs album oldSize justLoadedImages readyToDisplayImages ->
                            let
                                model =
                                    Thumbs album (Debug.log "window size updated for thumbs" size) justLoadedImages readyToDisplayImages

                                urls =
                                    AlbumPage.urlsToGet model
                            in
                            ( LoadedAlbum model parents flags home <|
                                Dict.union pendingUrls <|
                                    dictWithValues urls UrlRequested
                            , getUrls pendingUrls urls
                            )

                        FullImage album index loaded oldSize dragInfo ->
                            ( LoadedAlbum (FullImage album index loaded (Debug.log "window size updated for full" size) dragInfo) parents flags home pendingUrls
                            , Cmd.none
                            )

                LoadedList (AlbumListPage albumList oldSize parentLists) flags home pendingUrls ->
                    ( LoadedList (AlbumListPage albumList size parentLists) flags home pendingUrls
                    , Cmd.none
                    )

        YesHome home ->
            case model of
                LoadingHomeLink size flags path ->
                    gotHome size flags path <| Just home

                _ ->
                    ( model, Cmd.none )

        NoHome err ->
            case model of
                LoadingHomeLink size flags path ->
                    gotHome size flags path Nothing

                _ ->
                    ( model, Cmd.none )

        YesAlbum albumOrList ->
            case model of
                Loading winSize flags home paths ->
                    case albumOrList of
                        List albumList ->
                            let
                                newModel =
                                    LoadedList (AlbumListPage albumList winSize []) flags home Dict.empty
                            in
                            ( newModel
                            , pathsToCmd newModel paths
                            )

                        Leaf album ->
                            let
                                albumPage =
                                    Thumbs album winSize Set.empty Set.empty

                                urls =
                                    AlbumPage.urlsToGet albumPage

                                newModel =
                                    LoadedAlbum albumPage [] flags home <| dictWithValues urls UrlRequested
                            in
                            ( newModel
                            , Cmd.batch [ pathsToCmd newModel paths, getUrls Dict.empty urls ]
                            )

                _ ->
                    ( model, Cmd.none )

        NoAlbum err ->
            ( LoadError (flagsOf model) err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                LoadedAlbum oldPage parents flags home oldPendingUrls ->
                    let
                        ( newPage, newPageCmd ) =
                            AlbumPage.update pageMsg oldPage

                        newPendingUrls =
                            if AlbumPage.resetUrls pageMsg then
                                Dict.empty
                            else
                                oldPendingUrls

                        urls =
                            AlbumPage.urlsToGet newPage
                    in
                    ( LoadedAlbum newPage parents flags home <| Dict.union newPendingUrls <| dictWithValues urls UrlRequested
                    , Cmd.batch [ getUrls newPendingUrls urls, Cmd.map PageMsg newPageCmd ]
                    )

                _ ->
                    ( model, Cmd.none )

        ImageLoaded url ->
            updateImageResult model url JustCompleted

        ImageReadyToDisplay url ->
            updateImageResult model url ReadyToDisplay

        ImageFailed url err ->
            updateImageResult model url <| Failed err

        ViewList albumListPage ->
            let
                newModel =
                    LoadedList albumListPage (flagsOf model) (homeOf model) Dict.empty
            in
            ( newModel, scrollToTop )

        ViewAlbum albumPage parents ->
            let
                urls =
                    AlbumPage.urlsToGet albumPage

                newModel =
                    LoadedAlbum albumPage parents (flagsOf model) (homeOf model) <| dictWithValues urls UrlRequested
            in
            ( newModel
            , Cmd.batch [ scrollToTop, getUrls Dict.empty urls ]
            )

        ScrollSucceeded ->
            ( model, Cmd.none )

        ScrollFailed _ ->
            ( model, Cmd.none )

        Nav paths ->
            ( withPaths model paths, pathsToCmd model <| Just paths )

        NoBootstrap ->
            ( model, Cmd.none )


gotHome : WinSize -> AlbumBootstrapFlags -> Maybe (List String) -> Maybe String -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
gotHome size flags paths home =
    ( Loading size flags home paths
    , Task.attempt decodeAlbumRequest (Http.toTask (Http.get "album.json" jsonDecAlbumOrList))
    )


navToMsg : Location -> List AlbumBootstrapMsg
navToMsg loc =
    let
        parsedHash =
            Debug.log "parsedHash" <| parseHref loc.hash
    in
    case parsedHash of
        Err _ ->
            []

        Ok ( _, _, paths ) ->
            [ Nav paths ]


flagsOf : AlbumBootstrap -> AlbumBootstrapFlags
flagsOf model =
    case model of
        Sizing flags _ ->
            flags

        LoadingHomeLink _ flags _ ->
            flags

        Loading _ flags _ _ ->
            flags

        LoadError flags _ ->
            flags

        LoadedList _ flags _ _ ->
            flags

        LoadedAlbum _ _ flags _ _ ->
            flags


homeOf : AlbumBootstrap -> Maybe String
homeOf model =
    case model of
        Sizing _ _ ->
            Nothing

        LoadingHomeLink _ _ _ ->
            Nothing

        Loading _ _ home _ ->
            home

        LoadError _ _ ->
            Nothing

        LoadedList _ _ home _ ->
            home

        LoadedAlbum _ _ _ home _ ->
            home


withPaths : AlbumBootstrap -> List String -> AlbumBootstrap
withPaths model paths =
    case model of
        Sizing flags _ ->
            Sizing flags <| Just paths

        LoadingHomeLink winSize flags _ ->
            LoadingHomeLink winSize flags <| Just paths

        Loading winSize flags home _ ->
            Loading winSize flags home <| Just paths

        LoadError _ _ ->
            model

        LoadedList _ _ _ _ ->
            model

        LoadedAlbum _ _ _ _ _ ->
            model


pathsToCmd : AlbumBootstrap -> Maybe (List String) -> Cmd AlbumBootstrapMsg
pathsToCmd model mPaths =
    case mPaths of
        Nothing ->
            Cmd.none

        Just paths ->
            case model of
                Sizing _ _ ->
                    Cmd.none

                LoadingHomeLink _ _ _ ->
                    Cmd.none

                Loading _ _ _ _ ->
                    Cmd.none

                LoadError _ _ ->
                    Debug.log "pathsToCmd LoadError, ignore" Cmd.none

                LoadedList (AlbumListPage albumList winSize parents) _ _ _ ->
                    --TODO maybe don't always prepend aTN here, only if at root?
                    pathsToCmdImpl winSize (albumList :: parents) paths

                LoadedAlbum albumPage parents _ _ _ ->
                    pathsToCmdImpl (pageSize albumPage) parents paths


pathsToCmdImpl : WinSize -> List AlbumList -> List String -> Cmd AlbumBootstrapMsg
pathsToCmdImpl size parents paths =
    let
        mRoot =
            Debug.log "mRoot" <| List.head <| List.reverse parents
    in
    case mRoot of
        Nothing ->
            Cmd.none

        Just root ->
            navFrom size root [] paths <| cmdOf <| ViewList <| AlbumListPage root size []


navFrom : WinSize -> AlbumList -> List AlbumList -> List String -> Cmd AlbumBootstrapMsg -> Cmd AlbumBootstrapMsg
navFrom size root parents paths defcmd =
    case paths of
        [] ->
            defcmd

        [ "#" ] ->
            defcmd

        p :: ps ->
            let
                mChild =
                    findChild root p

                newParents =
                    root :: parents
            in
            case mChild of
                Nothing ->
                    defcmd

                Just pChild ->
                    case pChild of
                        List albumList ->
                            navFrom size albumList newParents ps <| cmdOf <| ViewList <| AlbumListPage albumList size newParents

                        Leaf album ->
                            navForAlbum size album ps newParents


navForAlbum : WinSize -> Album -> List String -> List AlbumList -> Cmd AlbumBootstrapMsg
navForAlbum size album ps newParents =
    case ps of
        [] ->
            cmdOf <| ViewAlbum (Thumbs album size Set.empty Set.empty) newParents

        i :: _ ->
            case findImg [] album i of
                Nothing ->
                    Cmd.none

                Just ( prevs, nAlbum ) ->
                    let
                        ( w, h ) =
                            fitImage nAlbum.imageFirst.srcSetFirst size.width size.height

                        ( progModel, progCmd ) =
                            progInit size nAlbum.imageFirst w h
                    in
                    Cmd.batch
                        [ cmdOf <| ViewAlbum (FullImage prevs nAlbum progModel size Nothing) newParents
                        , Cmd.map PageMsg <| Cmd.map FullMsg progCmd
                        ]


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
    Debug.log ("looking for " ++ name) <| List.head <| List.filter f <| containingList.childFirst :: containingList.childRest


cmdOf : a -> Cmd a
cmdOf msg =
    Task.perform (\_ -> msg) <| Task.succeed ()


locFor : AlbumBootstrap -> Maybe UrlChange
locFor model =
    case model of
        LoadedAlbum albumPage parents _ _ _ ->
            Just
                { entry = NewEntry
                , url = hashForAlbum model albumPage parents
                }

        LoadedList albumListPage _ _ _ ->
            Just
                { entry = NewEntry
                , url = hashForList model albumListPage
                }

        _ ->
            Nothing


hashForList : AlbumBootstrap -> AlbumListPage -> String
hashForList model (AlbumListPage albumList _ parents) =
    if List.isEmpty parents then
        hashFromAlbumPath model [ "" ] []
    else
        hashFromAlbumPath model [ albumList.listTitle ] parents


hashForAlbum : AlbumBootstrap -> AlbumPage -> List AlbumList -> String
hashForAlbum model albumPage parents =
    let
        titles =
            case albumPage of
                Thumbs album _ _ _ ->
                    [ album.title ]

                FullImage _ album _ _ _ ->
                    [ album.title, album.imageFirst.altText ]
    in
    hashFromAlbumPath model titles parents


hashFromAlbumPath : AlbumBootstrap -> List String -> List AlbumList -> String
hashFromAlbumPath model titles parents =
    "#"
        ++ String.concat
            (List.intersperse "/"
                (List.map
                    encodeUri
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
        Dom.Scroll.toTop rootDivId


updateImageResult : AlbumBootstrap -> String -> UrlLoadState -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
updateImageResult model url result =
    case model of
        LoadedAlbum albumPage parents flags home pendingUrls ->
            case albumPage of
                Thumbs album size justLoadedImages readyToDisplayImages ->
                    let
                        model =
                            justLoadedReadyToDisplayNextState album size justLoadedImages readyToDisplayImages url result

                        urls =
                            AlbumPage.urlsToGet model
                    in
                    ( LoadedAlbum model parents flags home <|
                        Dict.union (Dict.fromList [ ( url, result ) ]) <|
                            Dict.union pendingUrls <|
                                dictWithValues urls UrlRequested
                    , Cmd.batch
                        [ getUrls pendingUrls urls
                        , urlNextState url result
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


justLoadedReadyToDisplayNextState : Album -> WinSize -> Set String -> Set String -> String -> UrlLoadState -> AlbumPage
justLoadedReadyToDisplayNextState album size justLoadedImages readyToDisplayImages url result =
    case result of
        JustCompleted ->
            Thumbs album size (Set.insert url justLoadedImages) readyToDisplayImages

        ReadyToDisplay ->
            Thumbs album size (Set.remove url justLoadedImages) <| Set.insert url readyToDisplayImages

        _ ->
            Thumbs album size justLoadedImages readyToDisplayImages


urlNextState : String -> UrlLoadState -> Cmd AlbumBootstrapMsg
urlNextState url result =
    case result of
        JustCompleted ->
            Delay.after 100 millisecond <| ImageReadyToDisplay url

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
                , url = Debug.log "getUrl" url
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
        LoadedAlbum albumPage parents _ _ _ ->
            let
                showParent =
                    case parents of
                        [] ->
                            NoBootstrap

                        parent :: grandParents ->
                            ViewList <| AlbumListPage parent (pageSize albumPage) grandParents
            in
            Sub.batch
                [ AlbumPage.subscriptions albumPage PageMsg showParent
                , resizes Resize
                ]

        LoadedList (AlbumListPage albumList winSize parents) _ _ _ ->
            case parents of
                [] ->
                    resizes Resize

                parent :: grandParents ->
                    let
                        upParent =
                            onEscape
                                (ViewList <| AlbumListPage parent winSize grandParents)
                                NoBootstrap
                    in
                    Sub.batch [ upParent, resizes Resize ]

        _ ->
            resizes Resize


pageSize : AlbumPage -> WinSize
pageSize albumPage =
    case albumPage of
        Thumbs _ winSize _ _ ->
            winSize

        FullImage _ _ _ winSize _ ->
            winSize


view : AlbumBootstrap -> Html AlbumBootstrapMsg
view albumBootstrap =
    case albumBootstrap of
        Sizing _ _ ->
            text "Album Starting"

        LoadingHomeLink _ _ _ ->
            text "Home Loading ..."

        Loading _ _ _ _ ->
            text "Album Loading ..."

        LoadError _ e ->
            text ("Error Loading Album: " ++ toString e)

        LoadedAlbum albumPage parents flags home pendingUrls ->
            withHomeLink home <|
                AlbumPage.view
                    albumPage
                    (\list ->
                        ViewList <|
                            AlbumListPage
                                list
                                (pageSize albumPage)
                                (dropThrough parents list)
                    )
                    PageMsg
                    parents
                    flags

        LoadedList (AlbumListPage albumList winSize parents) flags home pendingUrls ->
            withHomeLink home <|
                AlbumListPage.view
                    (AlbumListPage
                        albumList
                        winSize
                        parents
                    )
                    (\albumListChild ->
                        ViewList <|
                            AlbumListPage albumListChild winSize <|
                                dropThrough
                                    (albumList
                                        :: parents
                                    )
                                    albumListChild
                    )
                    (\album ->
                        ViewAlbum
                            (Thumbs album winSize Set.empty Set.empty)
                        <|
                            albumList
                                :: parents
                    )
                    flags


withHomeLink : Maybe String -> Html AlbumBootstrapMsg -> Html AlbumBootstrapMsg
withHomeLink home basePage =
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
                    ]
                    [ text "⌂" ]
                ]

        Nothing ->
            basePage
