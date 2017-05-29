module ImageViews exposing (renderPresized, render)

import Album exposing (..)
import AlbumStyles exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (..)


-- TODO change signature to allow non-empty msg


renderPresized : Int -> Int -> Int -> ImgSrc -> List ImgSrc -> List Mixin -> List (Html.Attribute msg) -> msg -> Html msg
renderPresized margin w h i iRest s otherAttrs msg =
    render (smallestImageBiggerThan w h i iRest)
        []
        (s
            ++ [ Css.margin (px <| toFloat margin)
               , Css.width (px (toFloat <| w - 2 * margin))
               , Css.height (px (toFloat <| h - 2 * margin))
               ]
        )
        otherAttrs
        msg


smallestImageBiggerThan : Int -> Int -> ImgSrc -> List ImgSrc -> ImgSrc
smallestImageBiggerThan w h i iRest =
    case List.head <| List.sortBy (\is -> is.x) <| List.filter (\is -> is.x > w && is.y > h) <| i :: iRest of
        Nothing ->
            i

        Just sizedIs ->
            sizedIs


render : ImgSrc -> List ImgSrc -> List Mixin -> List (Html.Attribute msg) -> msg -> Html msg
render idefault is s otherAttrs msg =
    img
        ([ styles s
         , Html.Attributes.src idefault.url
         , attribute "srcset" (encodeSrcSet is)
         , Html.Attributes.width idefault.x
         , Html.Attributes.height idefault.y
         , onClick msg
         ]
            ++ otherAttrs
        )
        []


encodeSrcSet : List ImgSrc -> String
encodeSrcSet is =
    String.join ", " (List.map encodeSrc is)


encodeSrc : ImgSrc -> String
encodeSrc is =
    is.url ++ " " ++ (toString is.x) ++ "w"
