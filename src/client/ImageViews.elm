module ImageViews exposing (render, renderPresized, smallestImageBiggerThan)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import ListUtils exposing (..)


renderPresized : Int -> Int -> Int -> ImgSrc -> List ImgSrc -> List Style -> List (Html.Styled.Attribute msg) -> Maybe msg -> Html msg
renderPresized margin w h i iRest s otherAttrs msg =
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
        msg


smallestImageBiggerThan : Int -> Int -> ImgSrc -> List ImgSrc -> ImgSrc
smallestImageBiggerThan w h i iRest =
    case List.head <| List.sortBy (\is -> is.x) <| List.filter (\is -> is.x >= w && is.y >= h) <| i :: iRest of
        Nothing ->
            --Debug.log ("no sm bigger than " ++ toString w ++ ", " ++ toString h) i
            i

        Just sizedIs ->
            --Debug.log ("sm bigger than " ++ toString w ++ " is " ++ toString sizedIs.x ++ " (" ++ sizedIs.url ++ ")") sizedIs
            sizedIs


render : ImgSrc -> List ImgSrc -> List Style -> List (Html.Styled.Attribute msg) -> Maybe msg -> Html msg
render idefault is s otherAttrs msg =
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

        clickAttr =
            case msg of
                Just m ->
                    [ onClick m ]

                Nothing ->
                    []
    in
    img
        (baseAttrs ++ otherAttrs ++ srcset ++ clickAttr)
        []


encodeSrcSet : List ImgSrc -> String
encodeSrcSet is =
    String.join ", " (List.map encodeSrc is)


encodeSrc : ImgSrc -> String
encodeSrc is =
    encodePath is.url ++ " " ++ toString is.x ++ "w"
