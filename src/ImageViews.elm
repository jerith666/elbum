module ImageViews exposing (renderPresized, render)

import Album exposing (..)
import AlbumStyles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (..)

renderPresized : Int -> Int -> List ImgSrc -> List Mixin -> msg -> Html msg
renderPresized w h is s msg =
    case List.head <| List.sortBy (\is -> is.x) <| List.filter (\is -> is.x > w && is.y > h) is of
        Nothing ->
            div [] []
        Just sizedIs ->
            render sizedIs [] (s ++ [Css.width (px (toFloat w)), Css.height (px (toFloat h))]) msg

render : ImgSrc -> List ImgSrc -> List Mixin -> msg -> Html msg
render idefault is s msg =
    img
        [ styles s
        , Html.Attributes.src idefault.url
        , attribute "srcset" (encodeSrcSet is)
        , Html.Attributes.width idefault.x
        , Html.Attributes.height idefault.y
        , onClick msg
        ]
        []


encodeSrcSet : List ImgSrc -> String
encodeSrcSet is =
  String.join ", " (List.map encodeSrc is)


encodeSrc : ImgSrc -> String
encodeSrc is =
  is.url ++ " " ++ (toString is.x) ++ "w"

