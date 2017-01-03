module ThumbPage exposing (ThumbPageModel, view)

import Album exposing (..)
import WinSize exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Css exposing (..)

type alias ThumbPageModel =
    { album : Album
    , winSize : WinSize
    }

view : (Int -> msg) -> ThumbPageModel -> Html msg
view imgChosenMsgr thumbPageModel =
    rootDivFlexCol
        [ backgroundColor black ]
        <| viewThumbs 0 imgChosenMsgr thumbPageModel

viewThumbs : Int -> (Int -> msg) -> ThumbPageModel -> List (Html msg)
viewThumbs i imgChosenMsgr thumbPageModel =
    case List.head <| List.drop i thumbPageModel.album.images of
        Just nextImg ->
            viewThumb (imgChosenMsgr i) nextImg
            :: viewThumbs (i+1) imgChosenMsgr thumbPageModel

        Nothing ->
            []


viewThumb : msg -> Image -> Html msg
viewThumb selectedMsg img = Html.text "TODO view"

-- TODO move below to utils file

black =
    rgb 0 0 0

styles =
    Css.asPairs >> Html.Attributes.style

rootDiv extraStyles =
    div
        [ styles <|
            [ position absolute
            , Css.height (vh 100)
            , Css.width (vw 100)
            -- , overflow auto
            , backgroundColor black
            ]
            ++ extraStyles
        ]

rootDivFlexCol extraStyles =
    rootDiv <|
        [ displayFlex
        , flexDirection column
        ]
        ++ extraStyles

-- div [onClick (imgChosenMsgr 1)] [ Html.text ("Thumb Page for " ++ thumbPageModel.album.title ++ " at " ++ (toString thumbPageModel.winSize.width) ++ "x" ++ (toString thumbPageModel.winSize.height)) ]

