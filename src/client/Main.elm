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
import Title exposing (..)
import WinSize exposing (..)
import Window exposing (..)


type AlbumBootstrap
    = Sizing AlbumBootstrapFlags (Maybe (List String)) (Maybe Float)
    | LoadingHomeLink WinSize AlbumBootstrapFlags (Maybe (List String)) (Maybe Float)
    | Loading WinSize AlbumBootstrapFlags (Maybe String) (Maybe (List String)) (Maybe Float)
    | LoadError AlbumBootstrapFlags Http.Error
    | LoadedList AlbumListPage AlbumBootstrapFlags (Maybe String) (Dict String UrlLoadState) (Maybe Float)
    | LoadedAlbum AlbumPage (List ( AlbumList, Maybe Float )) AlbumBootstrapFlags (Maybe String) (Dict String UrlLoadState) (Maybe Float)
    | GettingScrollBootstrap AlbumBootstrap (Maybe Float -> AlbumBootstrap -> ( AlbumBootstrap, AlbumBootstrapMsg ))


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
    | ViewList AlbumListPage (Maybe Float)
    | ViewAlbum AlbumPage (List ( AlbumList, Maybe Float ))
    | ImageLoaded String
    | ImageReadyToDisplay String
    | ImageFailed String Http.Error
    | GetScroll AlbumBootstrap (Maybe Float -> AlbumBootstrap -> ( AlbumBootstrap, AlbumBootstrapMsg ))
    | GotScroll (Maybe Float)
    | ScrolledTo Float
    | ScrollSucceeded
    | ScrollFailed Id
    | Nav (List String)
    | Scroll Float
    | Sequence (Cmd AlbumBootstrapMsg) (List (Cmd AlbumBootstrapMsg))
    | NoBootstrap
    | LogScroll Float


main : RouteUrlProgram AlbumBootstrapFlags AlbumBootstrap AlbumBootstrapMsg
main =
    RouteUrl.programWithFlags
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        , delta2url = locFor
        , location2messages = navToMsg
        }


init : AlbumBootstrapFlags -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
init flags =
    ( Sizing flags Nothing Nothing
    , Task.perform Resize Window.size
    )


update : AlbumBootstrapMsg -> AlbumBootstrap -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
update msg model =
    case msg of
        Resize size ->
            case model of
                Sizing flags paths scroll ->
                    ( LoadingHomeLink (Debug.log "window size set" size) flags paths scroll
                    , Http.send (either NoHome YesHome) <| Http.getString "home"
                    )

                LoadingHomeLink oldSize flags paths scroll ->
                    ( LoadingHomeLink size flags paths scroll
                    , Cmd.none
                    )

                Loading oldSize flags home paths scroll ->
                    ( Loading (Debug.log "window size updated during load" size) flags home paths scroll
                    , Cmd.none
                    )

                LoadError _ _ ->
                    ( model, Cmd.none )

                GettingScrollBootstrap prevModel scrollUpdater ->
                    let
                        ( newPrevModel, _ ) =
                            update msg prevModel
                    in
                    ( GettingScrollBootstrap newPrevModel scrollUpdater, Cmd.none )

                LoadedAlbum albumPage parents flags home pendingUrls scrollPos ->
                    case albumPage of
                        GettingScroll _ _ _ _ _ underlyingModel ->
                            update msg <| LoadedAlbum underlyingModel parents flags home pendingUrls scrollPos

                        Thumbs album oldSize justLoadedImages readyToDisplayImages ->
                            let
                                model =
                                    Thumbs album (Debug.log "window size updated for thumbs" size) justLoadedImages readyToDisplayImages

                                urls =
                                    AlbumPage.urlsToGet model
                            in
                            ( LoadedAlbum model
                                parents
                                flags
                                home
                                (Dict.union pendingUrls <|
                                    dictWithValues urls UrlRequested
                                )
                                scrollPos
                            , getUrls pendingUrls urls
                            )

                        FullImage album index loaded oldSize savedScroll dragInfo ->
                            ( LoadedAlbum (FullImage album index loaded (Debug.log "window size updated for full" size) savedScroll dragInfo) parents flags home pendingUrls scrollPos
                            , Cmd.none
                            )

                LoadedList (AlbumListPage albumList oldSize parentLists) flags home pendingUrls scrollPos ->
                    ( LoadedList (AlbumListPage albumList size parentLists) flags home pendingUrls scrollPos
                    , Cmd.none
                    )

        YesHome home ->
            case model of
                LoadingHomeLink size flags path scroll ->
                    gotHome size flags path scroll <| Just home

                _ ->
                    ( model, Cmd.none )

        NoHome err ->
            case model of
                LoadingHomeLink size flags path scroll ->
                    gotHome size flags path scroll Nothing

                _ ->
                    ( model, Cmd.none )

        YesAlbum albumOrList ->
            case model of
                Loading winSize flags home paths scroll ->
                    let
                        scrollCmd =
                            Maybe.withDefault Cmd.none <| Maybe.map (\s -> Task.attempt (\_ -> NoBootstrap) <| toY rootDivId <| Debug.log "startup scroll to " s) scroll
                    in
                    case albumOrList of
                        List albumList ->
                            let
                                newModel =
                                    LoadedList (AlbumListPage albumList winSize []) flags home Dict.empty Nothing

                                pathsThenScroll =
                                    toCmd <| Sequence (pathsToCmd newModel paths) [ scrollCmd ]
                            in
                            ( newModel
                            , Cmd.batch
                                [ setTitle albumList.listTitle
                                , pathsThenScroll
                                ]
                            )

                        Leaf album ->
                            let
                                albumPage =
                                    Thumbs album winSize Set.empty Set.empty

                                urls =
                                    AlbumPage.urlsToGet albumPage

                                newModel =
                                    LoadedAlbum albumPage [] flags home (dictWithValues urls UrlRequested) Nothing

                                pathsThenScroll =
                                    toCmd <| Sequence (pathsToCmd newModel paths) [ scrollCmd ]
                            in
                            ( newModel
                            , Cmd.batch
                                [ getUrls Dict.empty urls
                                , setTitle album.title
                                , pathsThenScroll
                                ]
                            )

                _ ->
                    ( model, Cmd.none )

        NoAlbum err ->
            ( LoadError (flagsOf model) err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                LoadedAlbum oldPage parents flags home oldPendingUrls scrollPos ->
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
                    ( LoadedAlbum newPage parents flags home (Dict.union newPendingUrls <| dictWithValues urls UrlRequested) scrollPos
                    , Cmd.batch
                        [ getUrls newPendingUrls urls
                        , Cmd.map PageMsg newPageCmd
                        , setTitle <| titleOf newPage
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
                    LoadedList albumListPage (flagsOf model) (homeOf model) Dict.empty Nothing

                scrollCmd =
                    case maybeScroll of
                        Just pos ->
                            Task.attempt (\_ -> NoBootstrap) <| toY rootDivId pos

                        Nothing ->
                            scrollToTop

                title =
                    case albumListPage of
                        AlbumListPage albumList _ _ ->
                            albumList.listTitle
            in
            ( newModel
            , Cmd.batch [ scrollCmd, setTitle title ]
            )

        ViewAlbum albumPage parents ->
            let
                urls =
                    AlbumPage.urlsToGet albumPage

                newModel =
                    LoadedAlbum albumPage parents (flagsOf model) (homeOf model) (dictWithValues urls UrlRequested) Nothing
            in
            ( newModel
            , Cmd.batch
                [ scrollToTop
                , getUrls Dict.empty urls
                , setTitle <| titleOf albumPage
                ]
            )

        GetScroll oldModel scrollUpdater ->
            ( GettingScrollBootstrap oldModel scrollUpdater
            , attempt (GotScroll << Result.toMaybe) <| y rootDivId
            )

        GotScroll scroll ->
            case model of
                GettingScrollBootstrap prevModel scrollUpdater ->
                    let
                        ( model, msg ) =
                            scrollUpdater scroll prevModel
                    in
                    ( model, cmdOf msg )

                _ ->
                    ( model, Cmd.none )

        ScrolledTo pos ->
            ( withScrollPos pos model, Cmd.none )

        ScrollSucceeded ->
            ( model, Cmd.none )

        ScrollFailed _ ->
            ( model, Cmd.none )

        Scroll s ->
            ( withScroll model s, Cmd.none )

        Nav paths ->
            ( withPaths model paths, pathsToCmd model <| Just paths )

        Sequence next rest ->
            let
                cmds =
                    case rest of
                        [] ->
                            next

                        r1 :: rs ->
                            Cmd.batch [ next, toCmd <| Sequence r1 rs ]
            in
            ( model, cmds )

        NoBootstrap ->
            ( model, Cmd.none )

        LogScroll s ->
            case Debug.log "scroll from port " s of
                _ ->
                    ( model, Cmd.none )


gotHome : WinSize -> AlbumBootstrapFlags -> Maybe (List String) -> Maybe Float -> Maybe String -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
gotHome size flags paths scroll home =
    ( Loading size flags home paths scroll
    , Task.attempt decodeAlbumRequest (Http.toTask (Http.get "album.json" jsonDecAlbumOrList))
    )


navToMsg : Location -> List AlbumBootstrapMsg
navToMsg loc =
    let
        parsedHash =
            Debug.log "parsedHash" <| parseHref loc.hash

        parsedQuery =
            Debug.log "parsedQuery" <| parseQuery loc.search

        hashMsgs =
            case parsedHash of
                Err _ ->
                    []

                Ok ( _, _, paths ) ->
                    [ Nav paths ]

        queryMsgs =
            case parsedQuery of
                Err _ ->
                    []

                Ok ( _, _, scroll ) ->
                    fromMaybe <|
                        Maybe.map Scroll (scroll |> Maybe.andThen (Result.toMaybe << String.toFloat))
    in
    hashMsgs ++ queryMsgs


flagsOf : AlbumBootstrap -> AlbumBootstrapFlags
flagsOf model =
    case model of
        Sizing flags _ _ ->
            flags

        LoadingHomeLink _ flags _ _ ->
            flags

        Loading _ flags _ _ _ ->
            flags

        LoadError flags _ ->
            flags

        GettingScrollBootstrap prevModel _ ->
            flagsOf prevModel

        LoadedList _ flags _ _ _ ->
            flags

        LoadedAlbum _ _ flags _ _ _ ->
            flags


homeOf : AlbumBootstrap -> Maybe String
homeOf model =
    case model of
        Sizing _ _ _ ->
            Nothing

        LoadingHomeLink _ _ _ _ ->
            Nothing

        Loading _ _ home _ _ ->
            home

        LoadError _ _ ->
            Nothing

        GettingScrollBootstrap prevModel _ ->
            homeOf prevModel

        LoadedList _ _ home _ _ ->
            home

        LoadedAlbum _ _ _ home _ _ ->
            home


withScrollPos : Float -> AlbumBootstrap -> AlbumBootstrap
withScrollPos pos model =
    case model of
        Sizing _ _ _ ->
            model

        LoadingHomeLink _ _ _ _ ->
            model

        Loading _ _ _ _ _ ->
            model

        LoadError _ _ ->
            model

        GettingScrollBootstrap _ _ ->
            model

        LoadedAlbum albumPage parents flags home pendingUrls _ ->
            LoadedAlbum albumPage parents flags home pendingUrls <| Just pos

        LoadedList (AlbumListPage albumList winSize parents) flags home pendingUrls _ ->
            LoadedList (AlbumListPage albumList winSize parents) flags home pendingUrls <| Just pos


withPaths : AlbumBootstrap -> List String -> AlbumBootstrap
withPaths model paths =
    case model of
        Sizing flags _ scroll ->
            Sizing flags (Just paths) scroll

        LoadingHomeLink winSize flags _ scroll ->
            LoadingHomeLink winSize flags (Just paths) scroll

        Loading winSize flags home _ scroll ->
            Loading winSize flags home (Just paths) scroll

        LoadError _ _ ->
            model

        GettingScrollBootstrap _ _ ->
            model

        LoadedList _ _ _ _ _ ->
            model

        LoadedAlbum _ _ _ _ _ _ ->
            model


withScroll : AlbumBootstrap -> Float -> AlbumBootstrap
withScroll model scroll =
    case model of
        Sizing flags paths _ ->
            Sizing flags paths <| Just scroll

        LoadingHomeLink winSize flags paths _ ->
            LoadingHomeLink winSize flags paths <| Just scroll

        Loading winSize flags home paths _ ->
            Loading winSize flags home paths <| Just scroll

        LoadError _ _ ->
            model

        GettingScrollBootstrap _ _ ->
            model

        LoadedList _ _ _ _ _ ->
            model

        LoadedAlbum _ _ _ _ _ _ ->
            model


pathsToCmd : AlbumBootstrap -> Maybe (List String) -> Cmd AlbumBootstrapMsg
pathsToCmd model mPaths =
    case mPaths of
        Nothing ->
            Cmd.none

        Just paths ->
            case model of
                Sizing _ _ _ ->
                    Cmd.none

                LoadingHomeLink _ _ _ _ ->
                    Cmd.none

                Loading _ _ _ _ _ ->
                    Cmd.none

                LoadError _ _ ->
                    Debug.log "pathsToCmd LoadError, ignore" Cmd.none

                GettingScrollBootstrap _ _ ->
                    Debug.log "pathsToCmd GettingScrollBootstrap, ignore" Cmd.none

                LoadedList (AlbumListPage albumList winSize parents) _ _ _ _ ->
                    --TODO maybe don't always prepend aTN here, only if at root?
                    --TODO I think it's okay to drop the scroll positions here, should only happen at initial load (?)
                    pathsToCmdImpl winSize (albumList :: List.map Tuple.first parents) paths

                LoadedAlbum albumPage parents _ _ _ _ ->
                    pathsToCmdImpl (pageSize albumPage) (List.map Tuple.first parents) paths


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
            navFrom size root [] paths <| cmdOf <| ViewList (AlbumListPage root size []) Nothing


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
                            navFrom size albumList newParents ps <| cmdOf <| ViewList (AlbumListPage albumList size <| List.map (\p -> ( p, Nothing )) newParents) Nothing

                        Leaf album ->
                            navForAlbum size album ps newParents


navForAlbum : WinSize -> Album -> List String -> List AlbumList -> Cmd AlbumBootstrapMsg
navForAlbum size album ps newParents =
    let
        parentsNoScroll =
            List.map (\p -> ( p, Nothing )) newParents
    in
    case ps of
        [] ->
            cmdOf <| ViewAlbum (Thumbs album size Set.empty Set.empty) parentsNoScroll

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
                        [ cmdOf <| ViewAlbum (FullImage prevs nAlbum progModel size Nothing Nothing) parentsNoScroll
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


locFor : AlbumBootstrap -> AlbumBootstrap -> Maybe UrlChange
locFor oldModel newModel =
    let
        model =
            newModel

        entry =
            case oldModel of
                LoadedList _ _ _ _ _ ->
                    case newModel of
                        LoadedList _ _ _ _ _ ->
                            ModifyEntry

                        _ ->
                            NewEntry

                LoadedAlbum _ _ _ _ _ _ ->
                    case newModel of
                        LoadedAlbum _ _ _ _ _ _ ->
                            ModifyEntry

                        _ ->
                            NewEntry

                _ ->
                    NewEntry
    in
    case model of
        LoadedAlbum albumPage parents _ _ _ _ ->
            Just
                { entry = entry
                , url = queryFor model ++ (hashForAlbum model albumPage <| List.map Tuple.first parents)
                }

        LoadedList albumListPage _ _ _ _ ->
            Just
                { entry = entry
                , url = queryFor model ++ hashForList model albumListPage
                }

        _ ->
            Nothing


queryFor : AlbumBootstrap -> String
queryFor model =
    let
        queryForPos pos =
            Maybe.withDefault "" <| Maybe.map (\p -> "?s=" ++ toString p) pos
    in
    case model of
        Sizing _ _ _ ->
            ""

        LoadingHomeLink _ _ _ _ ->
            ""

        Loading _ _ _ _ _ ->
            ""

        LoadError _ _ ->
            ""

        GettingScrollBootstrap _ _ ->
            ""

        LoadedAlbum _ _ _ _ _ pos ->
            queryForPos pos

        LoadedList _ _ _ _ pos ->
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
                GettingScroll album _ _ _ _ _ ->
                    [ album.title ]

                Thumbs album _ _ _ ->
                    [ album.title ]

                FullImage _ album _ _ _ _ ->
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
        LoadedAlbum albumPage parents flags home pendingUrls scrollPos ->
            case albumPage of
                Thumbs album size justLoadedImages readyToDisplayImages ->
                    let
                        model =
                            justLoadedReadyToDisplayNextState album size justLoadedImages readyToDisplayImages url result

                        urls =
                            AlbumPage.urlsToGet model
                    in
                    ( LoadedAlbum model
                        parents
                        flags
                        home
                        (Dict.union (Dict.fromList [ ( url, result ) ]) <|
                            Dict.union pendingUrls <|
                                dictWithValues urls UrlRequested
                        )
                        scrollPos
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
                , url = Debug.log "getUrl" <| encodePath url
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
        LoadedAlbum albumPage parents _ _ _ _ ->
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
                , resizes Resize
                ]

        LoadedList (AlbumListPage albumList winSize parents) _ _ _ _ ->
            case parents of
                [] ->
                    resizes Resize

                ( parent, scroll ) :: grandParents ->
                    let
                        upParent =
                            onEscape
                                (ViewList (AlbumListPage parent winSize grandParents) scroll)
                                NoBootstrap
                    in
                    Sub.batch
                        [ upParent
                        , resizes Resize
                        ]

        _ ->
            resizes Resize


pageSize : AlbumPage -> WinSize
pageSize albumPage =
    case albumPage of
        GettingScroll _ _ _ _ winSize _ ->
            winSize

        Thumbs _ winSize _ _ ->
            winSize

        FullImage _ _ _ winSize _ _ ->
            winSize


view : AlbumBootstrap -> Html AlbumBootstrapMsg
view albumBootstrap =
    case albumBootstrap of
        Sizing _ _ _ ->
            text "Album Starting"

        LoadingHomeLink _ _ _ _ ->
            text "Home Loading ..."

        Loading _ _ _ _ _ ->
            text "Album Loading ..."

        LoadError _ e ->
            text ("Error Loading Album: " ++ toString e)

        GettingScrollBootstrap prevModel _ ->
            view prevModel

        LoadedAlbum albumPage parents flags home pendingUrls scrollPos ->
            withHomeLink home flags <|
                AlbumPage.view
                    albumPage
                    ScrolledTo
                    (viewList
                        albumBootstrap
                        (pageSize albumPage)
                        -- currently scrolled thing is the album;
                        -- don't want to save that anywhere in the list of parents
                        (\_ -> parents)
                    )
                    PageMsg
                    (List.map Tuple.first parents)
                    flags

        LoadedList (AlbumListPage albumList winSize parents) flags home pendingUrls scrollPos ->
            withHomeLink home flags <|
                AlbumListPage.view
                    (AlbumListPage
                        albumList
                        winSize
                        parents
                    )
                    (viewList
                        albumBootstrap
                        winSize
                        (\maybeScroll -> ( albumList, maybeScroll ) :: parents)
                    )
                    (\album ->
                        GetScroll albumBootstrap <|
                            \maybeScroll ->
                                \oldModel ->
                                    ( oldModel
                                    , ViewAlbum
                                        (Thumbs album winSize Set.empty Set.empty)
                                      <|
                                        ( albumList, maybeScroll )
                                            :: parents
                                    )
                    )
                    ScrolledTo
                    flags


viewList : AlbumBootstrap -> WinSize -> (Maybe Float -> List ( AlbumList, Maybe Float )) -> AlbumList -> AlbumBootstrapMsg
viewList oldModel winSize makeParents list =
    GetScroll oldModel <|
        \maybeScroll ->
            \oldModel2 ->
                ( oldModel2
                , ViewList
                    (AlbumListPage list winSize <|
                        dropThroughPred
                            (\( p, _ ) -> p == list)
                            (makeParents maybeScroll)
                    )
                    (if List.member list (List.map Tuple.first <| makeParents maybeScroll) then
                        --we're navigating up to a parent
                        --look up and use saved scroll position of that parent
                        case List.head <| List.filter (\e -> list == Tuple.first e) <| makeParents maybeScroll of
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
