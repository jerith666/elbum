module ImageViews exposing (renderPresized, smallestImageBiggerThan)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Utils.ListUtils exposing (..)


renderPresized : Int -> Int -> Int -> ImgSrc -> List ImgSrc -> List Style -> List (Html.Styled.Attribute msg) -> Html msg
renderPresized margin w h i iRest s otherAttrs =
    render (smallestImageBiggerThan w h i iRest)
        --empty list disables use of srcsets; experiments indicate they don't really work
        []
        ([ Css.margin (px <| toFloat margin)
         , Css.width (px (toFloat <| w - 2 * margin))
         , Css.height (px (toFloat <| h - 2 * margin))
         ]
            ++ s
        )
        otherAttrs


smallestImageBiggerThan : Int -> Int -> ImgSrc -> List ImgSrc -> ImgSrc
smallestImageBiggerThan w h i iRest =
    case List.head <| List.sortBy .x <| List.filter (\is -> is.x >= w && is.y >= h) <| i :: iRest of
        Nothing ->
            i

        Just sizedIs ->
            sizedIs


render : ImgSrc -> List ImgSrc -> List Style -> List (Html.Styled.Attribute msg) -> Html msg
render idefault is s otherAttrs =
    let
        srcset =
            case is of
                [] ->
                    []

                _ ->
                    [ attribute "srcset" (encodeSrcSet is) ]

        baseAttrs =
            [ styles s
            , Html.Styled.Attributes.src <| encodePath idefault.url
            , Html.Styled.Attributes.width idefault.x
            , Html.Styled.Attributes.height idefault.y
            ]
    in
    img
        (baseAttrs ++ otherAttrs ++ srcset)
        []


encodeSrcSet : List ImgSrc -> String
encodeSrcSet is =
    String.join ", " (List.map encodeSrc is)


encodeSrc : ImgSrc -> String
encodeSrc is =
    encodePath is.url ++ " " ++ String.fromInt is.x ++ "w"
