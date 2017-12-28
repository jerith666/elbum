module FullImagePage exposing (FullImagePageModel, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import ImageViews exposing (..)
import TouchEvents exposing (..)
import WinSize exposing (..)


type alias FullImagePageModel =
    { prevImgs : List Image
    , album : Album
    , winSize : WinSize
    , offset : ( Float, Float )
    }


imgTitleHeight : Float
imgTitleHeight =
    5


view : msg -> msg -> msg -> (Touch -> msg) -> (Touch -> msg) -> (Touch -> msg) -> msg -> FullImagePageModel -> AlbumBootstrapFlags -> Html msg
view prevMsg nextMsg backToThumbsMsg touchStartMsg touchContinueMsg touchPrevNextMsg noOpMsg fullImagePageModel flags =
    rootDivFlex
        flags
        column
        [ overflow hidden
        , alignItems center
        , property "justify-content" "center"
        ]
    <|
        [ div
            [ styles
                [ color white
                , textAlign center
                , height (pct imgTitleHeight)
                , lineHeight (px (imgTitleHeight / 100 * toFloat fullImagePageModel.winSize.height))
                ]
            ]
            [ Html.text fullImagePageModel.album.imageFirst.altText ]
        , viewImg nextMsg touchStartMsg touchContinueMsg touchPrevNextMsg fullImagePageModel fullImagePageModel.album.imageFirst
        ]
            ++ navEltIf fullImagePageModel.prevImgs prevMsg "<" left
            ++ navEltIf fullImagePageModel.album.imageRest nextMsg ">" right
            ++ [ div
                    [ styles <|
                        navBoxStyles
                            ++ [ top (px 5)
                               , right (px 5)
                               ]
                    , onClick backToThumbsMsg
                    ]
                    [ Html.text "x" ]
               ]


navEltIf : List a -> msg -> String -> (Px -> Mixin) -> List (Html msg)
navEltIf lst navMsg navTxt navAlign =
    if List.isEmpty lst then
        []
    else
        [ navElement navMsg navTxt navAlign ]


navEltSize : number
navEltSize =
    50


navBoxStyles : List Mixin
navBoxStyles =
    [ position absolute
    , height (px navEltSize)
    , width (px navEltSize)
    , lineHeight (px navEltSize)
    , textAlign center
    , color white
    , backgroundColor (rgba 40 40 40 0.5)
    , borderRadius (px <| navEltSize / 2)
    , cursor pointer
    ]


navElement : msg -> String -> (Px -> Mixin) -> Html msg
navElement msg label side =
    div
        [ styles <|
            navBoxStyles
                ++ [ side (px 0)
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
            toFloat winWidth / toFloat winHeight

        imgAspect =
            toFloat is.x / toFloat is.y

        scale =
            if winAspect <= imgAspect then
                toFloat winWidth / toFloat is.x
            else
                toFloat winHeight / toFloat is.y
    in
    ( Basics.round <| toFloat is.x * scale
    , Basics.round <| toFloat is.y * scale
    )