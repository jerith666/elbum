import WinSize exposing (..)
import Album exposing (..)
import AlbumPage exposing (..)

import Task exposing (..)
import Html exposing (..)
import Http exposing (..)
import Window exposing (..)

type AlbumBootstrap
    = Sizing
    | Loading WinSize
    | LoadError Http.Error
    | Loaded AlbumPage

type AlbumBootstrapMsg
    = Resize Size
    | YesAlbum Album
    | NoAlbum Http.Error
    | PageMsg AlbumPage.AlbumPageMsg

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
                    ( Loading size
                    , Task.attempt decodeAlbumRequest (Http.toTask (Http.get "album.json" jsonDecAlbum))
                    )

                Loading oldSize ->
                    ( Loading size
                    , Cmd.none
                    )

                LoadError _ -> ( model, Cmd.none )

                Loaded albumPage ->
                    case albumPage of
                        Thumbs album oldSize ->
                            ( Loaded (Thumbs album size)
                            , Cmd.none
                            )

                        FullImage album index oldSize ->
                            ( Loaded (FullImage album index size)
                            , Cmd.none
                            )

        YesAlbum album ->
            case model of
                Loading winSize ->
                    ( Loaded (Thumbs album winSize)
                    , Cmd.none
                    )

                _ -> ( model, Cmd.none )

        NoAlbum err ->
            ( LoadError err
            , Cmd.none
            )

        PageMsg pageMsg ->
            case model of
                Loaded oldPage ->
                    ( Loaded (AlbumPage.update pageMsg oldPage)
                    , Cmd.none
                    )

                _ -> ( model, Cmd.none )


decodeAlbumRequest : Result Http.Error Album -> AlbumBootstrapMsg
decodeAlbumRequest r =
    case r of
        Ok a ->
            YesAlbum a

        Err e ->
            NoAlbum e


subscriptions model = Sub.none --TODO FullImagePage.prevNextSubscriptions?


view : AlbumBootstrap -> Html AlbumBootstrapMsg
view albumBootstrap =
    case albumBootstrap of
        Sizing ->
            text "Album Starting"

        Loading _ ->
            text "Album Loading ..."

        LoadError e ->
            text ("Error Loading Album: " ++ (toString e))

        Loaded albumPage ->
            Html.map PageMsg (AlbumPage.view albumPage)

