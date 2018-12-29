module FullImagePage exposing (FullImagePageModel, fitImage, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import Css exposing (..)
import Html exposing (Attribute)
import Html.Events.Extra.Touch exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import ImageViews exposing (..)
import ProgressiveImage exposing (..)
import ThumbPage exposing (..)
import Utils.ListUtils exposing (..)
import Utils.TouchUtils exposing (..)


type alias FullImagePageModel =
    { prevImgs : List Image
    , album : Album
    , viewport : Viewport
    , progImgModel : ProgressiveImageModel
    , offset : Offset
    , imgPosition : Maybe Element
    }


type alias NavMsgs msg =
    { prevMsg : msg
    , nextMsg : msg
    , backToThumbsMsg : msg
    , showList : AlbumList -> msg
    }


type alias TouchMsgs msg =
    { touchStartMsg : Event -> msg
    , touchContinueMsg : Event -> msg
    , touchPrevNextMsg : Event -> msg
    }


imgTitleHeight : Float
imgTitleHeight =
    5


view : NavMsgs msg -> TouchMsgs msg -> msg -> (ProgressiveImageMsg -> msg) -> FullImagePageModel -> List AlbumList -> AlbumBootstrapFlags -> Html msg
view navMsgs touchMsgs noOpMsg wrapProgMsg fullImagePageModel parents flags =
    let
        xOfY =
            " ("
                ++ (String.fromInt <| 1 + List.length fullImagePageModel.prevImgs)
                ++ " of "
                ++ (String.fromInt <| List.length fullImagePageModel.prevImgs + 1 + List.length fullImagePageModel.album.imageRest)
                ++ ")"
    in
    rootDivFlex
        flags
        column
        Nothing
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
                , lineHeight (px (imgTitleHeight / 100 * fullImagePageModel.viewport.viewport.height))
                ]
            ]
            [ albumTitle
                (fullImagePageModel.album.imageFirst.altText ++ xOfY)
                parents
                navMsgs.showList
                [ albumParent getAlbumTitle (\_ -> navMsgs.backToThumbsMsg) fullImagePageModel.album ]
                []
            ]
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
                    , href <| encodePath fullImagePageModel.album.imageFirst.srcSetFirst.url
                    , Html.Styled.Attributes.target "_blank"
                    ]
                    [ Html.Styled.text "⤓" ]
               ]


getAlbumTitle : Album -> String
getAlbumTitle a =
    a.title


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
                (floor fullImagePageModel.viewport.viewport.width)
            <|
                Basics.round (fullImagePageModel.viewport.viewport.height * (1 - imgTitleHeight / 100))

        imgSrc =
            smallestImageBiggerThan w h img.srcSetFirst img.srcSetRest
    in
    div
        (offsetStyles fullImagePageModel.imgPosition fullImagePageModel.offset
            ++ [ Html.Styled.Attributes.fromUnstyled <| onTouch "start" touchMsgs.touchStartMsg
               , Html.Styled.Attributes.fromUnstyled <| onTouch "move" touchMsgs.touchContinueMsg
               , Html.Styled.Attributes.fromUnstyled <| onTouch "end" touchMsgs.touchPrevNextMsg
               , onClick clickMsg
               , id theImageId
               ]
        )
        [ Html.Styled.map wrapProgMsg <| ProgressiveImage.view <| withWidthHeight w h fullImagePageModel.progImgModel ]


offsetStyles : Maybe Element -> Offset -> List (Html.Styled.Attribute msg)
offsetStyles imgPosition offset =
    let
        posStyle =
            case offset of
                NoOffset ->
                    []

                Swipe x ->
                    -- note: no up/down movement is desired, just left/right
                    -- , top <| px <| Tuple.second fullImagePageModel.offset
                    [ left <| px x ]

                Zoom z ->
                    case imgPosition of
                        Nothing ->
                            --UGH TODO
                            []

                        Just imgPos ->
                            let
                                imgVpPos =
                                    ( imgPos.element.x - imgPos.viewport.x
                                    , imgPos.element.y - imgPos.viewport.y
                                    )

                                offsetPos =
                                    applyOffset offset imgVpPos
                            in
                            [ top <| px <| Tuple.second offsetPos
                            , left <| px <| Tuple.first offsetPos
                            , transform <| scale z.scale
                            ]
    in
    [ styles <| position relative :: posStyle ]


onTouch : String -> (Event -> msg) -> Html.Attribute msg
onTouch touchType =
    -- default is { stopPropagation = False, preventDefault = True }
    onWithOptions ("touch" ++ touchType) { stopPropagation = False, preventDefault = True }


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
