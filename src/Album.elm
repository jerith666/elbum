module Album exposing(..)

import Json.Decode
import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set


type alias AlbumTreeNode  =
   { nodeTitle: String
   , childFirst: NodeOrAlbum
   , childRest: (List NodeOrAlbum)
   }

jsonDecAlbumTreeNode : Json.Decode.Decoder ( AlbumTreeNode )
jsonDecAlbumTreeNode =
   ("nodeTitle" := Json.Decode.string) >>= \pnodeTitle ->
   ("childFirst" := jsonDecNodeOrAlbum) >>= \pchildFirst ->
   ("childRest" := Json.Decode.list (jsonDecNodeOrAlbum)) >>= \pchildRest ->
   Json.Decode.succeed {nodeTitle = pnodeTitle, childFirst = pchildFirst, childRest = pchildRest}

jsonEncAlbumTreeNode : AlbumTreeNode -> Value
jsonEncAlbumTreeNode  val =
   Json.Encode.object
   [ ("nodeTitle", Json.Encode.string val.nodeTitle)
   , ("childFirst", jsonEncNodeOrAlbum val.childFirst)
   , ("childRest", (Json.Encode.list << List.map jsonEncNodeOrAlbum) val.childRest)
   ]



type NodeOrAlbum  =
    Subtree AlbumTreeNode
    | Leaf Album

jsonDecNodeOrAlbum : Json.Decode.Decoder ( NodeOrAlbum )
jsonDecNodeOrAlbum =
    let jsonDecDictNodeOrAlbum = Dict.fromList
            [ ("Subtree", Json.Decode.map Subtree (jsonDecAlbumTreeNode))
            , ("Leaf", Json.Decode.map Leaf (jsonDecAlbum))
            ]
    in  decodeSumObjectWithSingleField  "NodeOrAlbum" jsonDecDictNodeOrAlbum

jsonEncNodeOrAlbum : NodeOrAlbum -> Value
jsonEncNodeOrAlbum  val =
    let keyval v = case v of
                    Subtree v1 -> ("Subtree", encodeValue (jsonEncAlbumTreeNode v1))
                    Leaf v1 -> ("Leaf", encodeValue (jsonEncAlbum v1))
    in encodeSumObjectWithSingleField keyval val



type alias Album  =
   { title: String
   , imageFirst: Image
   , imageRest: (List Image)
   }

jsonDecAlbum : Json.Decode.Decoder ( Album )
jsonDecAlbum =
   ("title" := Json.Decode.string) >>= \ptitle ->
   ("imageFirst" := jsonDecImage) >>= \pimageFirst ->
   ("imageRest" := Json.Decode.list (jsonDecImage)) >>= \pimageRest ->
   Json.Decode.succeed {title = ptitle, imageFirst = pimageFirst, imageRest = pimageRest}

jsonEncAlbum : Album -> Value
jsonEncAlbum  val =
   Json.Encode.object
   [ ("title", Json.Encode.string val.title)
   , ("imageFirst", jsonEncImage val.imageFirst)
   , ("imageRest", (Json.Encode.list << List.map jsonEncImage) val.imageRest)
   ]



type alias Image  =
   { altText: String
   , srcSetFirst: ImgSrc
   , srcSetRest: (List ImgSrc)
   }

jsonDecImage : Json.Decode.Decoder ( Image )
jsonDecImage =
   ("altText" := Json.Decode.string) >>= \paltText ->
   ("srcSetFirst" := jsonDecImgSrc) >>= \psrcSetFirst ->
   ("srcSetRest" := Json.Decode.list (jsonDecImgSrc)) >>= \psrcSetRest ->
   Json.Decode.succeed {altText = paltText, srcSetFirst = psrcSetFirst, srcSetRest = psrcSetRest}

jsonEncImage : Image -> Value
jsonEncImage  val =
   Json.Encode.object
   [ ("altText", Json.Encode.string val.altText)
   , ("srcSetFirst", jsonEncImgSrc val.srcSetFirst)
   , ("srcSetRest", (Json.Encode.list << List.map jsonEncImgSrc) val.srcSetRest)
   ]



type alias ImgSrc  =
   { url: String
   , x: Int
   , y: Int
   }

jsonDecImgSrc : Json.Decode.Decoder ( ImgSrc )
jsonDecImgSrc =
   ("url" := Json.Decode.string) >>= \purl ->
   ("x" := Json.Decode.int) >>= \px ->
   ("y" := Json.Decode.int) >>= \py ->
   Json.Decode.succeed {url = purl, x = px, y = py}

jsonEncImgSrc : ImgSrc -> Value
jsonEncImgSrc  val =
   Json.Encode.object
   [ ("url", Json.Encode.string val.url)
   , ("x", Json.Encode.int val.x)
   , ("y", Json.Encode.int val.y)
   ]


