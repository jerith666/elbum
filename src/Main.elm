import Album exposing (..)

type AlbumBootstrap
    = Sizing
    | Loading WinSize
    | LoadError Http.Error
    | Loaded AlbumPage

type AlbumBootstrapMsg
    = Resize Size
    | YesAlbum Album
    | NoAlbum Http.Error

view : AlbumBootstrap -> Html AlbumBootstrapMsg
view albumBoostrap =
    case albumBootstrap of
        Sizing ->
            text "Album Starting"

        Loading _ ->
            text "Album Loading ..."

        LoadError e ->
            text "Error Loading Album: " ++ (toString e)

        Loaded albumPage ->
            AlbumPage.view albumPage

