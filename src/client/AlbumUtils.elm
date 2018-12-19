module AlbumUtils exposing (albumJson, findChild, findImg, hashFromAlbumPath)

import Album exposing (..)
import Url exposing (..)


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
        f albumOrList =
            case albumOrList of
                List albumList ->
                    albumList.listTitle == name

                Leaf album ->
                    album.title == name
    in
    List.head <| List.filter f <| containingList.childFirst :: containingList.childRest


hashFromAlbumPath : List String -> List AlbumList -> String
hashFromAlbumPath titles parents =
    String.concat
        (List.intersperse "/"
            (List.map
                percentEncode
                (List.append
                    (List.map
                        (\p -> p.listTitle)
                        (List.drop 1 (List.reverse parents))
                    )
                    titles
                )
            )
        )
