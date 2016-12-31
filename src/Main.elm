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
                    , Cmd.none --TODO get album.json
                    )

                Loading oldSize ->
                    ( Loading size
                    , Cmd.none
                    )

                LoadError _ -> ( model, Cmd.none )

                Loaded albumPage ->
                    case albumPage of
                        Thumbs album oldSize ->
                            ( Thumbs album size
                            , Cmd.none
                            )

                        FullImage album index oldSize ->
                            ( FullImage album index size
                            , Cmd.none
                            )

        YesAlbum album ->
            case model of
                Loading winSize ->
                    ( Loaded Thumbs album winSize
                    , Cmd.none
                    )

                _ -> ( model, Cmd.none )

        NoAlbum err ->
                ( LoadError err
                , Cmd.none
                )


subscriptions = Sub.none --TODO FullImagePage.prevNextSubscriptions?


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

