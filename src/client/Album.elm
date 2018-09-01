module Album exposing (Album, AlbumList, AlbumOrList(..), Image, ImgSrc, jsonDecAlbum, jsonDecAlbumList, jsonDecAlbumOrList, jsonDecImage, jsonDecImgSrc, jsonEncAlbum, jsonEncAlbumList, jsonEncAlbumOrList, jsonEncImage, jsonEncImgSrc)

-- The following module comes from bartavelle/json-helpers

import Dict
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set


type alias AlbumList =
    { listTitle : String
    , childFirst : AlbumOrList
    , childRest : List AlbumOrList
    , listThumbnail : Image
    }


jsonDecAlbumList : Json.Decode.Decoder AlbumList
jsonDecAlbumList =
    ("listTitle" := Json.Decode.string)
        >>= (\plistTitle ->
                ("childFirst" := jsonDecAlbumOrList)
                    >>= (\pchildFirst ->
                            ("childRest" := Json.Decode.list jsonDecAlbumOrList)
                                >>= (\pchildRest ->
                                        ("listThumbnail" := jsonDecImage)
                                            >>= (\plistThumbnail ->
                                                    Json.Decode.succeed { listTitle = plistTitle, childFirst = pchildFirst, childRest = pchildRest, listThumbnail = plistThumbnail }
                                                )
                                    )
                        )
            )


jsonEncAlbumList : AlbumList -> Value
jsonEncAlbumList val =
    Json.Encode.object
        [ ( "listTitle", Json.Encode.string val.listTitle )
        , ( "childFirst", jsonEncAlbumOrList val.childFirst )
        , ( "childRest", (Json.Encode.list << List.map jsonEncAlbumOrList) val.childRest )
        , ( "listThumbnail", jsonEncImage val.listThumbnail )
        ]


type AlbumOrList
    = List AlbumList
    | Leaf Album


jsonDecAlbumOrList : Json.Decode.Decoder AlbumOrList
jsonDecAlbumOrList =
    let
        jsonDecDictAlbumOrList =
            Dict.fromList
                [ ( "List", Json.Decode.lazy (\_ -> Json.Decode.map List jsonDecAlbumList) )
                , ( "Leaf", Json.Decode.lazy (\_ -> Json.Decode.map Leaf jsonDecAlbum) )
                ]
    in
    decodeSumObjectWithSingleField "AlbumOrList" jsonDecDictAlbumOrList


jsonEncAlbumOrList : AlbumOrList -> Value
jsonEncAlbumOrList val =
    let
        keyval v =
            case v of
                List v1 ->
                    ( "List", encodeValue (jsonEncAlbumList v1) )

                Leaf v1 ->
                    ( "Leaf", encodeValue (jsonEncAlbum v1) )
    in
    encodeSumObjectWithSingleField keyval val


type alias Album =
    { title : String
    , thumbnail : Image
    , imageFirst : Image
    , imageRest : List Image
    }


jsonDecAlbum : Json.Decode.Decoder Album
jsonDecAlbum =
    ("title" := Json.Decode.string)
        >>= (\ptitle ->
                ("thumbnail" := jsonDecImage)
                    >>= (\pthumbnail ->
                            ("imageFirst" := jsonDecImage)
                                >>= (\pimageFirst ->
                                        ("imageRest" := Json.Decode.list jsonDecImage)
                                            >>= (\pimageRest ->
                                                    Json.Decode.succeed { title = ptitle, thumbnail = pthumbnail, imageFirst = pimageFirst, imageRest = pimageRest }
                                                )
                                    )
                        )
            )


jsonEncAlbum : Album -> Value
jsonEncAlbum val =
    Json.Encode.object
        [ ( "title", Json.Encode.string val.title )
        , ( "thumbnail", jsonEncImage val.thumbnail )
        , ( "imageFirst", jsonEncImage val.imageFirst )
        , ( "imageRest", (Json.Encode.list << List.map jsonEncImage) val.imageRest )
        ]


type alias Image =
    { altText : String
    , srcSetFirst : ImgSrc
    , srcSetRest : List ImgSrc
    }


jsonDecImage : Json.Decode.Decoder Image
jsonDecImage =
    ("altText" := Json.Decode.string)
        >>= (\paltText ->
                ("srcSetFirst" := jsonDecImgSrc)
                    >>= (\psrcSetFirst ->
                            ("srcSetRest" := Json.Decode.list jsonDecImgSrc)
                                >>= (\psrcSetRest ->
                                        Json.Decode.succeed { altText = paltText, srcSetFirst = psrcSetFirst, srcSetRest = psrcSetRest }
                                    )
                        )
            )


jsonEncImage : Image -> Value
jsonEncImage val =
    Json.Encode.object
        [ ( "altText", Json.Encode.string val.altText )
        , ( "srcSetFirst", jsonEncImgSrc val.srcSetFirst )
        , ( "srcSetRest", (Json.Encode.list << List.map jsonEncImgSrc) val.srcSetRest )
        ]


type alias ImgSrc =
    { url : String
    , x : Int
    , y : Int
    }


jsonDecImgSrc : Json.Decode.Decoder ImgSrc
jsonDecImgSrc =
    ("url" := Json.Decode.string)
        >>= (\purl ->
                ("x" := Json.Decode.int)
                    >>= (\px ->
                            ("y" := Json.Decode.int)
                                >>= (\py ->
                                        Json.Decode.succeed { url = purl, x = px, y = py }
                                    )
                        )
            )


jsonEncImgSrc : ImgSrc -> Value
jsonEncImgSrc val =
    Json.Encode.object
        [ ( "url", Json.Encode.string val.url )
        , ( "x", Json.Encode.int val.x )
        , ( "y", Json.Encode.int val.y )
        ]
