module FullImagePage exposing (FullImagePageModel, view)

import Album exposing (..)
import WinSize exposing (..)
import ImageViews exposing (..)
import AlbumStyles exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Css exposing (..)
import TouchEvents exposing (..)


type alias FullImagePageModel =
    { prevImgs : List Image
    , album : Album
    , winSize : WinSize
    , offset : ( Float, Float )
    }


imgTitleHeight : Float
imgTitleHeight =
    5


view : msg -> msg -> msg -> (Touch -> msg) -> (Touch -> msg) -> (Touch -> msg) -> msg -> FullImagePageModel -> Html msg
view prevMsg nextMsg backToThumbsMsg touchStartMsg touchContinueMsg touchPrevNextMsg noOpMsg fullImagePageModel =
    rootDivFlex column
        [ overflow hidden
        , alignItems center
        , property "justify-content" "center"
        ]
    <|
        (if List.isEmpty fullImagePageModel.prevImgs then
            []
         else
            [ navElement prevMsg "<" left ]
        )
            ++ (if List.isEmpty fullImagePageModel.album.imageRest then
                    []
                else
                    [ navElement nextMsg ">" right ]
               )
            ++ [ div
                    [ styles
                        [ position absolute
                        , top (px 5)
                        , right (px 5)
                        , color white
                        ]
                    , onClick backToThumbsMsg
                    ]
                    [ Html.text "x" ]
               , div
                    [ styles
                        [ color white
                        , textAlign center
                        , height (pct imgTitleHeight)
                        , lineHeight (px (imgTitleHeight / 100 * toFloat fullImagePageModel.winSize.height))
                        ]
                    ]
                    [ Html.text fullImagePageModel.album.imageFirst.altText ]
               , viewImg noOpMsg touchStartMsg touchContinueMsg touchPrevNextMsg fullImagePageModel fullImagePageModel.album.imageFirst
               ]


navElement msg label side =
    div
        [ styles
            [ position absolute
            , height (vh 100)
            , lineHeight (vh 100)
            , side (px 0)
            , width (pct 5)
            , textAlign center
            , color white
            , backgroundColor (rgba 40 40 40 0.5)
            ]
        , onClick msg
        ]
        [ Html.text label ]


viewImg : msg -> (Touch -> msg) -> (Touch -> msg) -> (Touch -> msg) -> FullImagePageModel -> Image -> Html msg
viewImg clickMsg touchStartMsg touchContinueMsg touchPrevNext fullImagePageModel img =
    let
        ( w, h ) =
            fitImage
                img.srcSetFirst
                fullImagePageModel.winSize.width
            <|
                Basics.round (toFloat fullImagePageModel.winSize.height * (1 - imgTitleHeight / 100))
    in
        renderPresized
            0
            w
            h
            img.srcSetFirst
            img.srcSetRest
            [ position relative
              -- note: no up/down movement is desired, just left/right
              -- , top <| px <| Tuple.second fullImagePageModel.offset
            , left <| px <| Tuple.first fullImagePageModel.offset
            ]
            [ onTouchStart touchStartMsg
            , onTouchMove touchContinueMsg
            , onTouchEnd touchPrevNext
            ]
        <|
            Just clickMsg


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
