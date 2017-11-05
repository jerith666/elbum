module Main exposing (..)

import WinSize exposing (..)
import Album exposing (..)
import AlbumPage exposing (..)
import AlbumTreeNodePage exposing (..)
import AlbumStyles exposing (..)
import ListUtils exposing (..)
import Task exposing (..)
import Html exposing (..)
import Http exposing (..)
import Window exposing (..)
import Set exposing (..)
import Dict exposing (..)
import Time exposing (..)
import Delay exposing (..)
import Dom.Scroll exposing (..)
import Dom exposing (..)
import Navigation exposing (..)


type AlbumBootstrap
    = Sizing Location
    | Loading Location WinSize
    | LoadError Location Http.Error
    | LoadedNode Location AlbumTreeNodePage (Dict String UrlLoadState)
    | LoadedAlbum Location AlbumPage (List AlbumTreeNode) (Dict String UrlLoadState)


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
    | Navigate Location


main : Program Never AlbumBootstrap AlbumBootstrapMsg
main =
    Navigation.program
        Navigate
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Location -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
init loc =
    ( Sizing loc
    , Task.perform Resize Window.size
    )


update : AlbumBootstrapMsg -> AlbumBootstrap -> ( AlbumBootstrap, Cmd AlbumBootstrapMsg )
update msg model =
    case msg of
        Resize size ->
            case model of
                Sizing loc ->
                    ( Loading loc <| Debug.log "window size set" size
                    , Task.attempt decodeAlbumRequest (Http.toTask (Http.get "album.json" jsonDecNodeOrAlbum))
                    )

                Loading loc oldSize ->
                    ( Loading loc <| Debug.log "window size updated during load" size
                    , Cmd.none
                    )

                LoadError _ _ ->
                    ( model, Cmd.none )

                LoadedAlbum loc albumPage parents pendingUrls ->
                    case albumPage of
                        Thumbs album oldSize justLoadedImages readyToDisplayImages ->
                            let
                                model =
                                    Thumbs album (Debug.log "window size updated for thumbs" size) justLoadedImages readyToDisplayImages

                                urls =
                                    AlbumPage.urlsToGet model
                            in
                                ( LoadedAlbum loc model parents <|
                                    Dict.union pendingUrls <|
                                        dictWithValues urls Requested
                                , getUrls pendingUrls urls
                                )

                        FullImage album index oldSize dragInfo ->
                            ( LoadedAlbum loc (FullImage album index (Debug.log "window size updated for full" size) dragInfo) parents pendingUrls
                            , Cmd.none
                            )

                LoadedNode loc (AlbumTreeNodePage albumNode oldSize parentNodes) pendingUrls ->
                    ( LoadedNode loc (AlbumTreeNodePage albumNode size parentNodes) pendingUrls
                    , Cmd.none
                    )

        YesAlbum nodeOrAlbum ->
            case model of
                Loading loc winSize ->
                    let
                        locate =
                            Task.perform (\_ -> Navigate loc) <| Task.succeed ()
                    in
                        case nodeOrAlbum of
                            Subtree albumNode ->
                                ( LoadedNode loc (AlbumTreeNodePage albumNode winSize []) Dict.empty
                                , locate
                                )

                            Leaf album ->
                                let
                                    albumPage =
                                        Thumbs album winSize Set.empty Set.empty

                                    urls =
                                        AlbumPage.urlsToGet albumPage
                                in
                                    ( LoadedAlbum loc albumPage [] <| dictWithValues urls Requested
                                    , Cmd.batch
                                        [ locate
                                        , getUrls Dict.empty urls
                                        ]
                                    )

                _ ->
                    ( model, Cmd.none )

        NoAlbum err ->
            ( LoadError (locOf model) err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                LoadedAlbum loc oldPage parents oldPendingUrls ->
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
                        ( LoadedAlbum loc newPage parents <| Dict.union newPendingUrls <| dictWithValues urls Requested
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
                newLoc =
                    locForNode model albumTreeNodePage
            in
                ( LoadedNode newLoc albumTreeNodePage Dict.empty
                , Cmd.batch [ scrollToTop, newUrl <| locToString newLoc ]
                )

        ViewAlbum albumPage parents ->
            let
                urls =
                    AlbumPage.urlsToGet albumPage

                newLoc =
                    locForAlbum model albumPage
            in
                ( LoadedAlbum newLoc albumPage parents <| dictWithValues urls Requested
                , Cmd.batch [ scrollToTop, newUrl <| locToString newLoc, getUrls Dict.empty urls ]
                )

        ScrollSucceeded ->
            ( model, Cmd.none )

        ScrollFailed _ ->
            ( model, Cmd.none )

        Navigate loc ->
            ( model, Cmd.none )


locForNode : AlbumBootstrap -> AlbumTreeNodePage -> Location
locForNode model _ =
    --TODO
    locOf model


locForAlbum : AlbumBootstrap -> AlbumPage -> Location
locForAlbum model _ =
    --TODO
    locOf model


locOf : AlbumBootstrap -> Location
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


locToString : Location -> String
locToString loc =
    --TODO
    loc.href


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
        LoadedAlbum loc albumPage parents pendingUrls ->
            case albumPage of
                Thumbs album size justLoadedImages readyToDisplayImages ->
                    let
                        model =
                            justLoadedReadyToDisplayNextState album size justLoadedImages readyToDisplayImages url result

                        urls =
                            AlbumPage.urlsToGet model
                    in
                        ( LoadedAlbum loc model parents <|
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
        LoadedAlbum loc albumPage parents pendingUrls ->
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
        Sizing _ ->
            text "Album Starting"

        Loading _ _ ->
            text "Album Loading ..."

        LoadError _ e ->
            text ("Error Loading Album: " ++ (toString e))

        LoadedAlbum loc albumPage parents pendingUrls ->
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

        LoadedNode loc (AlbumTreeNodePage albumTreeNode winSize parents) pendingUrls ->
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
