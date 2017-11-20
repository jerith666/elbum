module Main exposing (..)

--, parse, <*)
--import Combine exposing ((<*), ParseErr, ParseOk, end, parse, regex, sepBy, string)

import Album exposing (..)
import AlbumPage exposing (..)
import AlbumStyles exposing (..)
import AlbumTreeNodePage exposing (..)
import Delay exposing (..)
import Dict exposing (..)
import Dom exposing (..)
import Dom.Scroll exposing (..)
import Html exposing (..)
import Http exposing (..)
import ListUtils exposing (..)
import LocationUtils exposing (..)
import Navigation exposing (..)
import RouteUrl exposing (..)
import Set exposing (..)
import Task exposing (..)
import Time exposing (..)
import WinSize exposing (..)
import Window exposing (..)


type AlbumBootstrap
    = Sizing
    | Loading WinSize
    | LoadError Http.Error
    | LoadedNode AlbumTreeNodePage (Dict String UrlLoadState)
    | LoadedAlbum AlbumPage (List AlbumTreeNode) (Dict String UrlLoadState)


type UrlLoadState
    = Requested
      --| Partial Int
    | JustCompleted
    | ReadyToDisplay
    | Failed Http.Error


type AlbumBootstrapMsg
    = Resize Size
    | YesAlbum NodeOrAlbum
    | NoAlbum Http.Error
    | PageMsg AlbumPage.AlbumPageMsg
    | ViewNode AlbumTreeNodePage
    | ViewAlbum AlbumPage (List AlbumTreeNode)
    | ImageLoaded String
    | ImageReadyToDisplay String
    | ImageFailed String Http.Error
    | ScrollSucceeded
    | ScrollFailed Id
      --| Navigate Location
    | Nav (List String)


main : RouteUrlProgram Never AlbumBootstrap AlbumBootstrapMsg
main =
    RouteUrl.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , delta2url = \_ -> locFor
        , location2messages = navToMsg
        }


init : ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
init =
    ( Sizing
    , Task.perform Resize Window.size
    )


update : AlbumBootstrapMsg -> AlbumBootstrap -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
update msg model =
    case msg of
        Resize size ->
            case model of
                Sizing ->
                    ( Loading <| Debug.log "window size set" size
                    , Task.attempt decodeAlbumRequest (Http.toTask (Http.get "album.json" jsonDecNodeOrAlbum))
                    )

                Loading oldSize ->
                    ( Loading <| Debug.log "window size updated during load" size
                    , Cmd.none
                    )

                LoadError _ ->
                    ( model, Cmd.none )

                LoadedAlbum albumPage parents pendingUrls ->
                    case albumPage of
                        Thumbs album oldSize justLoadedImages readyToDisplayImages ->
                            let
                                model =
                                    Thumbs album (Debug.log "window size updated for thumbs" size) justLoadedImages readyToDisplayImages

                                urls =
                                    AlbumPage.urlsToGet model
                            in
                            ( LoadedAlbum model parents <|
                                Dict.union pendingUrls <|
                                    dictWithValues urls Requested
                            , getUrls pendingUrls urls
                            )

                        FullImage album index oldSize dragInfo ->
                            ( LoadedAlbum (FullImage album index (Debug.log "window size updated for full" size) dragInfo) parents pendingUrls
                            , Cmd.none
                            )

                LoadedNode (AlbumTreeNodePage albumNode oldSize parentNodes) pendingUrls ->
                    ( LoadedNode (AlbumTreeNodePage albumNode size parentNodes) pendingUrls
                    , Cmd.none
                    )

        YesAlbum nodeOrAlbum ->
            case model of
                Loading winSize ->
                    case nodeOrAlbum of
                        Subtree albumNode ->
                            ( LoadedNode (AlbumTreeNodePage albumNode winSize []) Dict.empty
                            , Cmd.none
                            )

                        Leaf album ->
                            let
                                albumPage =
                                    Thumbs album winSize Set.empty Set.empty

                                urls =
                                    AlbumPage.urlsToGet albumPage
                            in
                            ( LoadedAlbum albumPage [] <| dictWithValues urls Requested
                            , getUrls Dict.empty urls
                            )

                _ ->
                    ( model, Cmd.none )

        NoAlbum err ->
            ( LoadError err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                LoadedAlbum oldPage parents oldPendingUrls ->
                    let
                        newPage =
                            AlbumPage.update pageMsg oldPage

                        newPendingUrls =
                            if AlbumPage.resetUrls pageMsg then
                                Dict.empty
                            else
                                oldPendingUrls

                        urls =
                            AlbumPage.urlsToGet newPage
                    in
                    ( LoadedAlbum newPage parents <| Dict.union newPendingUrls <| dictWithValues urls Requested
                    , getUrls newPendingUrls urls
                    )

                _ ->
                    ( model, Cmd.none )

        ImageLoaded url ->
            updateImageResult model url JustCompleted

        ImageReadyToDisplay url ->
            updateImageResult model url ReadyToDisplay

        ImageFailed url err ->
            updateImageResult model url <| Failed err

        ViewNode albumTreeNodePage ->
            let
                newModel =
                    LoadedNode albumTreeNodePage Dict.empty
            in
            ( newModel, scrollToTop )

        ViewAlbum albumPage parents ->
            let
                urls =
                    AlbumPage.urlsToGet albumPage

                newModel =
                    LoadedAlbum albumPage parents <| dictWithValues urls Requested
            in
            ( newModel
            , Cmd.batch [ scrollToTop, getUrls Dict.empty urls ]
            )

        ScrollSucceeded ->
            ( model, Cmd.none )

        ScrollFailed _ ->
            ( model, Cmd.none )

        Nav paths ->
            ( model, pathsToCmd model paths )



--( withLoc model loc, navToMsg model loc )


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


pathsToCmd : AlbumBootstrap -> List String -> Cmd AlbumBootstrapMsg
pathsToCmd model paths =
    case model of
        Sizing ->
            Cmd.none

        Loading _ ->
            Cmd.none

        LoadError _ ->
            Cmd.none

        LoadedNode (AlbumTreeNodePage albumTreeNode winSize parents) _ ->
            --TODO maybe don't always prepend aTN here, only if at root?
            pathsToCmdImpl winSize (albumTreeNode :: parents) paths

        LoadedAlbum albumPage parents _ ->
            pathsToCmdImpl (pageSize albumPage) parents paths


pathsToCmdImpl : WinSize -> List AlbumTreeNode -> List String -> Cmd AlbumBootstrapMsg
pathsToCmdImpl size parents paths =
    let
        mRoot =
            Debug.log "mRoot" <| List.head <| List.reverse parents

        navFrom root paths defcmd =
            case paths of
                [] ->
                    defcmd

                p :: ps ->
                    let
                        mChild =
                            findChild root p
                    in
                    case mChild of
                        Nothing ->
                            defcmd

                        Just pChild ->
                            case pChild of
                                Subtree albumTreeNode ->
                                    navFrom albumTreeNode ps <| cmdOf <| ViewNode <| AlbumTreeNodePage albumTreeNode size parents

                                Leaf album ->
                                    cmdOf <| ViewAlbum (Thumbs album size Set.empty Set.empty) parents
    in
    case mRoot of
        Nothing ->
            Cmd.none

        Just root ->
            navFrom root paths Cmd.none


findChild : AlbumTreeNode -> String -> Maybe NodeOrAlbum
findChild containingNode name =
    let
        f nodeOrAlbum =
            case nodeOrAlbum of
                Subtree albumTreeNode ->
                    albumTreeNode.nodeTitle == name

                Leaf album ->
                    album.title == name
    in
    Debug.log ("looking for " ++ name) <| List.head <| List.filter f <| containingNode.childFirst :: containingNode.childRest


cmdOf : a -> Cmd a
cmdOf msg =
    Task.perform (\_ -> msg) <| Task.succeed ()


locFor : AlbumBootstrap -> Maybe UrlChange
locFor model =
    case model of
        LoadedAlbum albumPage parents _ ->
            Just <|
                { entry = NewEntry
                , url = hashForAlbum model albumPage parents
                }

        LoadedNode albumTreeNodePage _ ->
            Just <|
                { entry = NewEntry
                , url = hashForNode model albumTreeNodePage
                }

        _ ->
            Nothing


hashForNode : AlbumBootstrap -> AlbumTreeNodePage -> String
hashForNode model nodePage =
    case nodePage of
        AlbumTreeNodePage albumTreeNode _ parents ->
            if List.isEmpty parents then
                hashFromAlbumPath model "" []
            else
                hashFromAlbumPath model albumTreeNode.nodeTitle parents


hashForAlbum : AlbumBootstrap -> AlbumPage -> List AlbumTreeNode -> String
hashForAlbum model albumPage parents =
    let
        title =
            case albumPage of
                Thumbs album _ _ _ ->
                    album.title

                FullImage _ album _ _ ->
                    album.title
    in
    hashFromAlbumPath model title parents


hashFromAlbumPath : AlbumBootstrap -> String -> List AlbumTreeNode -> String
hashFromAlbumPath model title parents =
    "#"
        ++ String.concat
            (List.intersperse "/"
                (List.append
                    (List.map
                        (\p -> p.nodeTitle)
                        (List.drop 1 (List.reverse parents))
                    )
                    [ title ]
                )
            )



{- locOf : AlbumBootstrap -> Location
   locOf model =
       case model of
           Sizing loc ->
               loc

           Loading loc _ ->
               loc

           LoadError loc _ ->
               loc

           LoadedAlbum loc _ _ _ ->
               loc

           LoadedNode loc _ _ ->
               loc
-}
{- withLoc : AlbumBootstrap -> Location -> AlbumBootstrap
   withLoc model loc =
       case model of
           Sizing _ ->
               Sizing loc

           Loading _ winSize ->
               Loading loc winSize

           LoadError _ err ->
               LoadError loc err

           LoadedAlbum _ a b c ->
               LoadedAlbum loc a b c

           LoadedNode _ a b ->
               LoadedNode loc a b
-}


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
        toTop rootDivId


updateImageResult : AlbumBootstrap -> String -> UrlLoadState -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
updateImageResult model url result =
    case model of
        LoadedAlbum albumPage parents pendingUrls ->
            case albumPage of
                Thumbs album size justLoadedImages readyToDisplayImages ->
                    let
                        model =
                            justLoadedReadyToDisplayNextState album size justLoadedImages readyToDisplayImages url result

                        urls =
                            AlbumPage.urlsToGet model
                    in
                    ( LoadedAlbum model parents <|
                        Dict.union (Dict.fromList [ ( url, result ) ]) <|
                            Dict.union pendingUrls <|
                                dictWithValues urls Requested
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
            after 100 millisecond <| ImageReadyToDisplay url

        _ ->
            Cmd.none


decodeAlbumRequest : Result Http.Error NodeOrAlbum -> AlbumBootstrapMsg
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
        LoadedAlbum albumPage parents pendingUrls ->
            Sub.batch
                [ Sub.map PageMsg <| AlbumPage.subscriptions albumPage
                , resizes Resize
                ]

        _ ->
            resizes Resize



-- Sub.none --TODO FullImagePage.prevNextSubscriptions?


pageSize : AlbumPage -> WinSize
pageSize albumPage =
    case albumPage of
        Thumbs _ winSize _ _ ->
            winSize

        FullImage _ _ winSize _ ->
            winSize


view : AlbumBootstrap -> Html AlbumBootstrapMsg
view albumBootstrap =
    case albumBootstrap of
        Sizing ->
            text "Album Starting"

        Loading _ ->
            text "Album Loading ..."

        LoadError e ->
            text ("Error Loading Album: " ++ toString e)

        LoadedAlbum albumPage parents pendingUrls ->
            AlbumPage.view
                albumPage
                (\node ->
                    ViewNode <|
                        AlbumTreeNodePage
                            node
                            (pageSize albumPage)
                            (dropThrough parents node)
                )
                PageMsg
                parents

        LoadedNode (AlbumTreeNodePage albumTreeNode winSize parents) pendingUrls ->
            AlbumTreeNodePage.view
                (AlbumTreeNodePage
                    albumTreeNode
                    winSize
                    parents
                )
                (\albumTreeNodeChild ->
                    ViewNode <|
                        AlbumTreeNodePage albumTreeNodeChild winSize <|
                            dropThrough
                                (albumTreeNode
                                    :: parents
                                )
                                albumTreeNodeChild
                )
                (\album ->
                    ViewAlbum
                        (Thumbs album winSize Set.empty Set.empty)
                    <|
                        albumTreeNode
                            :: parents
                )
