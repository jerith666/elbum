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
            rootDiv [overflow hidden] [viewImg prevMsg nextMsg backToThumbsMsg fullImagePageModel img]


viewImg : msg -> msg -> msg -> FullImagePageModel -> Image -> Html msg
viewImg prevMsg nextMsg backToThumbsMsg fullImagePageModel img =
    case img.srcSet of
        [] ->
            div [] []

        is1 :: _ ->
            let
                (wStyle, hStyle) = aspectStyles fullImagePageModel is1
            in
                render is1 img.srcSet [wStyle, hStyle] nextMsg


aspectStyles : FullImagePageModel -> ImgSrc -> (Mixin, Mixin) -- (Css.LengthOrAuto compatible, Css.LengthOrAuto compatible)
aspectStyles fullImagePageModel is1 =
    let
        winAspect = (toFloat fullImagePageModel.winSize.width) / (toFloat fullImagePageModel.winSize.height)
        imgAspect = (toFloat is1.x) / (toFloat is1.y)
        wStyle = if winAspect > imgAspect then Css.width auto else Css.width (pct 100)
        hStyle = if winAspect > imgAspect then Css.height (pct 100) else Css.height auto
    in
        (wStyle, hStyle)

-- div [] [ Html.text ("Full Image Page for " ++ fullImagePageModel.album.title ++ " image " ++ (toString fullImagePageModel.index)) ]

