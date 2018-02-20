module FullImagePage exposing (FullImagePageModel, fitImage, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import ImageViews exposing (..)
import ProgressiveImage exposing (..)
import TouchEvents exposing (..)
import WinSize exposing (..)


type alias FullImagePageModel =
    { prevImgs : List Image
    , album : Album
    , winSize : WinSize
    , progImgModel : ProgressiveImageModel
    , offset : ( Float, Float )
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


view : NavMsgs msg -> TouchMsgs msg -> msg -> (ProgressiveImageMsg -> msg) -> FullImagePageModel -> AlbumBootstrapFlags -> Html msg
view navMsgs touchMsgs noOpMsg wrapProgMsg fullImagePageModel flags =
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
        , viewImg navMsgs.nextMsg touchMsgs wrapProgMsg fullImagePageModel
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
               , a
                    [ styles <| navBoxStyles ++ [ bottom (px 5), right (px 5), textDecoration none ]
                    , href fullImagePageModel.album.imageFirst.srcSetFirst.url
                    , Html.Styled.Attributes.target "_blank"
                    ]
                    [ Html.Styled.text "⤓" ]
               ]


navEltIf : List a -> msg -> String -> (Px -> Style) -> List (Html msg)
navEltIf lst navMsg navTxt navAlign =
    if List.isEmpty lst then
        []
    else
        [ navElement navMsg navTxt navAlign ]


viewImg : msg -> TouchMsgs msg -> (ProgressiveImageMsg -> msg) -> FullImagePageModel -> Html msg
viewImg clickMsg touchMsgs wrapProgMsg fullImagePageModel =
    let
        img =
            fullImagePageModel.album.imageFirst

        ( w, h ) =
            fitImage
                img.srcSetFirst
                fullImagePageModel.winSize.width
            <|
                Basics.round (toFloat fullImagePageModel.winSize.height * (1 - imgTitleHeight / 100))

        imgSrc =
            smallestImageBiggerThan w h img.srcSetFirst img.srcSetRest
    in
    div
        [ styles
            [ position relative

            -- note: no up/down movement is desired, just left/right
            -- , top <| px <| Tuple.second fullImagePageModel.offset
            , left <| px <| Tuple.first fullImagePageModel.offset
            ]
        , Html.Styled.Attributes.fromUnstyled <| onTouchStart touchMsgs.touchStartMsg
        , Html.Styled.Attributes.fromUnstyled <| onTouchMove touchMsgs.touchContinueMsg
        , Html.Styled.Attributes.fromUnstyled <| onTouchEnd touchMsgs.touchPrevNextMsg
        , onClick clickMsg
        ]
        [ Html.Styled.map wrapProgMsg <| ProgressiveImage.view fullImagePageModel.progImgModel ]


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
