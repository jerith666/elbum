module Main exposing (..)

import WinSize exposing (..)
import Album exposing (..)
import AlbumPage exposing (..)
import AlbumTreeNodePage exposing (..)
import Task exposing (..)
import Html exposing (..)
import Http exposing (..)
import Window exposing (..)


type AlbumBootstrap
    = Sizing
    | Loading WinSize
    | LoadError Http.Error
    | LoadedNode AlbumTreeNodePage
    | LoadedAlbum AlbumPage (Maybe AlbumTreeNode)


type AlbumBootstrapMsg
    = Resize Size
    | YesAlbum NodeOrAlbum
    | NoAlbum Http.Error
    | PageMsg AlbumPage.AlbumPageMsg


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

                LoadedAlbum albumPage parent ->
                    case albumPage of
                        Thumbs album oldSize ->
                            ( LoadedAlbum (Thumbs album <| Debug.log "window size updated for thumbs" size) parent
                            , Cmd.none
                            )

                        FullImage album index oldSize dragInfo ->
                            ( LoadedAlbum (FullImage album index (Debug.log "window size updated for full" size) dragInfo) parent
                            , Cmd.none
                            )

                LoadedNode (AlbumTreeNodePage albumNode oldSize parentNode) ->
                    ( LoadedNode (AlbumTreeNodePage albumNode size parentNode)
                    , Cmd.none
                    )

        YesAlbum nodeOrAlbum ->
            case model of
                Loading winSize ->
                    case nodeOrAlbum of
                        Subtree albumNode ->
                            ( LoadedNode (AlbumTreeNodePage albumNode winSize [])
                            , Cmd.none
                            )

                        Leaf album ->
                            ( LoadedAlbum (Thumbs album winSize) Nothing
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        NoAlbum err ->
            ( LoadError err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                LoadedAlbum oldPage parent ->
                    ( LoadedAlbum (AlbumPage.update pageMsg oldPage) parent
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


decodeAlbumRequest : Result Http.Error NodeOrAlbum -> AlbumBootstrapMsg
decodeAlbumRequest r =
    case r of
        Ok a ->
            YesAlbum a

        Err e ->
            NoAlbum e


subscriptions : AlbumBootstrap -> Sub AlbumBootstrapMsg
subscriptions model =
    case model of
        LoadedAlbum albumPage parent ->
            Sub.batch
                [ Sub.map PageMsg <| AlbumPage.subscriptions albumPage
                , resizes Resize
                ]

        _ ->
            resizes Resize



-- Sub.none --TODO FullImagePage.prevNextSubscriptions?


view : AlbumBootstrap -> Html AlbumBootstrapMsg
view albumBootstrap =
    case albumBootstrap of
        Sizing ->
            text "Album Starting"

        Loading _ ->
            text "Album Loading ..."

        LoadError e ->
            text ("Error Loading Album: " ++ (toString e))

        LoadedAlbum albumPage parent ->
            Html.map PageMsg (AlbumPage.view albumPage parent)

        LoadedNode albumNode ->
            AlbumTreeNodePage.view albumNode
