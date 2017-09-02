module Main exposing (..)

import WinSize exposing (..)
import Album exposing (..)
import AlbumPage exposing (..)
import AlbumTreeNodePage exposing (..)
import ListUtils exposing (..)
import Task exposing (..)
import Html exposing (..)
import Http exposing (..)
import Window exposing (..)
import Set exposing (..)
import AnimationTest exposing (..)


type AlbumBootstrap
    = Sizing
    | Loading WinSize
    | LoadError Http.Error
    | LoadedNode AlbumTreeNodePage (Set String)
    | LoadedAlbum AlbumPage (List AlbumTreeNode) (Set String)


type AlbumBootstrapMsg
    = Resize Size
    | YesAlbum NodeOrAlbum
    | NoAlbum Http.Error
    | PageMsg AlbumPage.AlbumPageMsg
    | ViewNode AlbumTreeNodePage
    | ViewAlbum AlbumPage (List AlbumTreeNode)
    | ImageLoaded String


main : Program Never AlbumBootstrap AlbumBootstrapMsg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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
                        Thumbs album oldSize justLoadedImages loadedImages ->
                            let
                                model =
                                    Thumbs album (Debug.log "window size updated for thumbs" size) justLoadedImages loadedImages

                                urls =
                                    AlbumPage.urlsToGet model
                            in
                                ( LoadedAlbum model parents <| union pendingUrls urls
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
                            ( LoadedNode (AlbumTreeNodePage albumNode winSize []) empty
                            , Cmd.none
                            )

                        Leaf album ->
                            let
                                albumPage =
                                    Thumbs album winSize empty

                                urls =
                                    AlbumPage.urlsToGet albumPage
                            in
                                ( LoadedAlbum albumPage [] urls
                                , getUrls empty urls
                                )

                _ ->
                    ( model, Cmd.none )

        NoAlbum err ->
            ( LoadError err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                LoadedAlbum oldPage parents pendingUrls ->
                    let
                        newPage =
                            AlbumPage.update pageMsg oldPage

                        urls =
                            AlbumPage.urlsToGet newPage
                    in
                        ( LoadedAlbum (newPage) parents <| union pendingUrls urls
                        , getUrls pendingUrls urls
                        )

                _ ->
                    ( model, Cmd.none )

        ImageLoaded url ->
            case model of
                LoadedAlbum albumPage parents pendingUrls ->
                    case albumPage of
                        Thumbs album size loadedImages ->
                            let
                                model =
                                    Thumbs album size <| insert url loadedImages

                                urls =
                                    AlbumPage.urlsToGet model
                            in
                                ( LoadedAlbum model parents <| union pendingUrls urls
                                  --TODO union wrong here?
                                , getUrls pendingUrls urls
                                )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ViewNode albumTreeNodePage ->
            ( LoadedNode albumTreeNodePage empty
            , Cmd.none
            )

        ViewAlbum albumPage parents ->
            let
                urls =
                    AlbumPage.urlsToGet albumPage
            in
                ( LoadedAlbum albumPage parents urls
                , getUrls empty urls
                )


decodeAlbumRequest : Result Http.Error NodeOrAlbum -> AlbumBootstrapMsg
decodeAlbumRequest r =
    case r of
        Ok a ->
            YesAlbum a

        Err e ->
            NoAlbum e


getUrls : Set String -> Set String -> Cmd AlbumBootstrapMsg
getUrls pending urls =
    Cmd.batch <| List.map getUrl <| Set.toList <| Set.diff urls pending


getUrl : String -> Cmd AlbumBootstrapMsg
getUrl url =
    Task.attempt decodeUrlResult
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


decodeUrlResult : Result Error String -> AlbumBootstrapMsg
decodeUrlResult result =
    case result of
        Ok url ->
            ImageLoaded url

        Err e ->
            NoAlbum e


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
            text ("Error Loading Album: " ++ (toString e))

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
                        (Thumbs album winSize empty)
                    <|
                        albumTreeNode
                            :: parents
                )
