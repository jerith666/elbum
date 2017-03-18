module Album exposing(..)

import Json.Decode
import Json.Decode exposing (field)
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set


type alias Album  =
   { title: String
   , imageFirst: Image
   , imageRest: (List Image)
   }

jsonDecAlbum : Json.Decode.Decoder ( Album )
jsonDecAlbum =
   (field "title" Json.Decode.string) |> Json.Decode.andThen (\ptitle ->
   (field "imageFirst" jsonDecImage) |> Json.Decode.andThen (\pimageFirst ->
   (field "imageRest" (Json.Decode.list (jsonDecImage))) |> Json.Decode.andThen (\pimageRest ->
   Json.Decode.succeed {title = ptitle, imageFirst = pimageFirst, imageRest = pimageRest})))

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
   (field "altText" Json.Decode.string) |> Json.Decode.andThen (\paltText ->
   (field "srcSetFirst" jsonDecImgSrc) |> Json.Decode.andThen (\psrcSetFirst ->
   (field "srcSetRest" (Json.Decode.list (jsonDecImgSrc))) |> Json.Decode.andThen (\psrcSetRest ->
   Json.Decode.succeed {altText = paltText, srcSetFirst = psrcSetFirst, srcSetRest = psrcSetRest})))

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
   (field "url" Json.Decode.string) |> Json.Decode.andThen (\purl ->
   (field "x" Json.Decode.int) |> Json.Decode.andThen (\px ->
   (field "y" Json.Decode.int) |> Json.Decode.andThen (\py ->
   Json.Decode.succeed {url = purl, x = px, y = py})))

jsonEncImgSrc : ImgSrc -> Value
jsonEncImgSrc  val =
   Json.Encode.object
   [ ("url", Json.Encode.string val.url)
   , ("x", Json.Encode.int val.x)
   , ("y", Json.Encode.int val.y)
   ]


