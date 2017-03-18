module ImageViews exposing (renderPresized, render)

import Album exposing (..)
import AlbumStyles exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (..)


-- TODO change signature to allow non-empty msg


renderPresized : Int -> Int -> Int -> List ImgSrc -> List Mixin -> List (Html.Attribute msg) -> msg -> Html msg
renderPresized margin w h is s otherAttrs msg =
    case List.head <| List.sortBy (\is -> is.x) <| List.filter (\is -> is.x > w && is.y > h) is of
        Nothing ->
            div [] []

        Just sizedIs ->
            render sizedIs
                []
                (s
                    ++ [ Css.margin (px <| toFloat margin)
                       , Css.width (px (toFloat <| w - 2 * margin))
                       , Css.height (px (toFloat <| h - 2 * margin))
                       ]
                )
                otherAttrs
                msg


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
