module FullImagePage exposing (FullImagePageModel, view)

import Album exposing (..)
import WinSize exposing (..)
import ImageViews exposing (..)
import AlbumStyles exposing (..)
import Html exposing (..)
import Css exposing (..)


type alias FullImagePageModel =
    { album : Album
    , index : Int
    , winSize : WinSize
    }


view : msg -> msg -> msg -> FullImagePageModel -> Html msg
view prevMsg nextMsg backToThumbsMsg fullImagePageModel =
    case List.head <| List.drop fullImagePageModel.index fullImagePageModel.album.images of
        Nothing ->
            div [] []

        Just img ->
            rootDiv
                [ overflow hidden ]
                [ div
                    [ styles [ color white
                             , textAlign center
                             ]
                    ]
                    [ Html.text img.altText ]
                , viewImg prevMsg nextMsg backToThumbsMsg fullImagePageModel img
                ]


viewImg : msg -> msg -> msg -> FullImagePageModel -> Image -> Html msg
viewImg prevMsg nextMsg backToThumbsMsg fullImagePageModel img =
    case img.srcSet of
        [] ->
            div [] []

        is1 :: _ ->
            let
                ( w, h ) =
                    fitImage is1 fullImagePageModel.winSize.width fullImagePageModel.winSize.height
            in
                renderPresized 0 w h img.srcSet [] nextMsg


fitImage : ImgSrc -> Int -> Int -> ( Int, Int )
fitImage is winWidth winHeight =
    let
        winAspect =
            (toFloat winWidth) / (toFloat winHeight)

        imgAspect =
            (toFloat is.x) / (toFloat is.y)

        scale =
            if winAspect <= imgAspect then
                (toFloat winWidth) / (toFloat is.x)
            else
                (toFloat winHeight) / (toFloat is.y)
    in
        ( Basics.round <| (toFloat is.x) * scale
        , Basics.round <| (toFloat is.y) * scale
        )
