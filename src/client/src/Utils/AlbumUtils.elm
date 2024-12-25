module Utils.AlbumUtils exposing (albumJson, findChild, findImg, pathFromAlbumPath)

import Album exposing (..)
import Utils.DebugSupport exposing (log)
import Utils.HttpUtils exposing (PercentEncoded, percentEncode)


albumJson : String
albumJson =
    "album.json"


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
        titleIsName albumOrList =
            case albumOrList of
                List albumList ->
                    log ("findChild list " ++ albumList.listTitle ++ " =?= " ++ name) <| albumList.listTitle == name

                Leaf album ->
                    log ("findChild leaf " ++ album.title ++ " =?= " ++ album.title) <| album.title == name
    in
    List.head <| List.filter titleIsName <| containingList.childFirst :: containingList.childRest


pathFromAlbumPath : List String -> List AlbumList -> List PercentEncoded
pathFromAlbumPath titles parents =
    List.map
        percentEncode
        (List.append
            (List.map
                (\p -> p.listTitle)
                (List.drop 1 (List.reverse parents))
            )
            titles
        )
