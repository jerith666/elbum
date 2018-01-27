module FullImagePage exposing (FullImagePageModel, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import ImageViews exposing (..)
import Json.Decode exposing (..)
import TouchEvents exposing (..)
import WinSize exposing (..)


type alias FullImagePageModel =
    { prevImgs : List Image
    , album : Album
    , winSize : WinSize
    , offset : ( Float, Float )
    , loaded : Bool
    }


type alias NavMsgs msg =
    { prevMsg : msg
    , nextMsg : msg
    , backToThumbsMsg : msg
    }


type alias TouchMsgs msg =
    { touchStartMsg : Touch -> msg
    , touchContinueMsg : Touch -> msg
    , touchPrevNextMsg : Touch -> msg
    }


imgTitleHeight : Float
imgTitleHeight =
    5


view : NavMsgs msg -> (String -> msg) -> TouchMsgs msg -> msg -> FullImagePageModel -> AlbumBootstrapFlags -> Html msg
view navMsgs loadedMsg touchMsgs noOpMsg fullImagePageModel flags =
    rootDivFlex
        flags
        column
        [ overflow Css.hidden
        , alignItems center
        , Css.property "justify-content" "center"
        ]
    <|
        [ div
            [ styles
                [ color white
                , textAlign center
                , Css.height (pct imgTitleHeight)
                , lineHeight (px (imgTitleHeight / 100 * toFloat fullImagePageModel.winSize.height))
                ]
            ]
            [ Html.Styled.text fullImagePageModel.album.imageFirst.altText ]
        , viewImg loadedMsg navMsgs.nextMsg touchMsgs fullImagePageModel fullImagePageModel.album.imageFirst fullImagePageModel.loaded
        ]
            ++ navEltIf fullImagePageModel.prevImgs navMsgs.prevMsg "<" left
            ++ navEltIf fullImagePageModel.album.imageRest navMsgs.nextMsg ">" right
            ++ [ div
                    [ styles <|
                        navBoxStyles
                            ++ [ top (px 5)
                               , right (px 5)
                               ]
                    , onClick navMsgs.backToThumbsMsg
                    ]
                    [ Html.Styled.text "x" ]
               ]


navEltIf : List a -> msg -> String -> (Px -> Style) -> List (Html msg)
navEltIf lst navMsg navTxt navAlign =
    if List.isEmpty lst then
        []
    else
        [ navElement navMsg navTxt navAlign ]


navEltSize : number
navEltSize =
    50


navBoxStyles : List Style
navBoxStyles =
    [ position absolute
    , Css.height (px navEltSize)
    , Css.width (px navEltSize)
    , lineHeight (px navEltSize)
    , textAlign center
    , color white
    , backgroundColor (rgba 40 40 40 0.5)
    , borderRadius (px <| navEltSize / 2)
    , cursor pointer
    ]


navElement : msg -> String -> (Px -> Style) -> Html msg
navElement msg label side =
    div
        [ styles <|
            navBoxStyles
                ++ [ side (px 0)
                   ]
        , onClick msg
        ]
        [ Html.Styled.text label ]


viewImg : (String -> msg) -> msg -> TouchMsgs msg -> FullImagePageModel -> Image -> Bool -> Html msg
viewImg loadedMsg clickMsg touchMsgs fullImagePageModel img loaded =
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
        ([ position relative

         -- note: no up/down movement is desired, just left/right
         -- , top <| px <| Tuple.second fullImagePageModel.offset
         , left <| px <| Tuple.first fullImagePageModel.offset
         ]
            ++ opacityStyles
                ( if loaded then
                    1.0
                  else
                    0.0
                , loaded
                )
        )
        , Html.Styled.Attributes.fromUnstyled <| onTouchStart touchMsgs.touchStartMsg
        , Html.Styled.Attributes.fromUnstyled <| onTouchMove touchMsgs.touchContinueMsg
        , Html.Styled.Attributes.fromUnstyled <| onTouchEnd touchMsgs.touchPrevNextMsg
        , on "load" <| succeed <| loadedMsg img.srcSetFirst.url
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
