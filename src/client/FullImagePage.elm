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
import ProgressiveImage exposing (..)
import ThumbPage exposing (..)
import Utils.ListUtils exposing (..)
import Utils.LocationUtils exposing (AnchorFunction)
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


view : AnchorFunction msg -> NavMsgs msg -> TouchMsgs msg -> (ProgressiveImageMsg -> msg) -> FullImagePageModel -> List AlbumList -> MainAlbumFlags -> Html msg
view a navMsgs touchMsgs wrapProgMsg fullImagePageModel parents flags =
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
        fullImagePageModel.viewport
        [ overflow Css.hidden
        , alignItems center
        , Css.justifyContent center
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
            [ albumTitle a
                (fullImagePageModel.album.imageFirst.altText ++ xOfY)
                parents
                navMsgs.showList
                [ albumParent a getAlbumTitle (always navMsgs.backToThumbsMsg) fullImagePageModel.album ]
                []
            ]
        , viewImg a navMsgs.nextMsg touchMsgs wrapProgMsg fullImagePageModel
        ]
            ++ navEltIf a fullImagePageModel.prevImgs navMsgs.prevMsg "<" left
            ++ navEltIf a fullImagePageModel.album.imageRest navMsgs.nextMsg ">" right
            ++ [ a navMsgs.backToThumbsMsg
                    [ styles <|
                        navBoxStyles
                            ++ [ top (px 5)
                               , right (px 5)
                               ]
                    ]
                    [ Html.Styled.text "x" ]
               , Html.Styled.a
                    [ styles <| navBoxStyles ++ [ top (px <| fullImagePageModel.viewport.viewport.height - navEltSize - 5), right (px 5), textDecoration none ]
                    , href <| encodePath fullImagePageModel.album.imageFirst.srcSetFirst.url
                    , Html.Styled.Attributes.target "_blank"
                    ]
                    [ Html.Styled.text "⤓" ]
               ]


getAlbumTitle : Album -> String
getAlbumTitle a =
    a.title


navEltIf : AnchorFunction msg -> List a -> msg -> String -> (Px -> Style) -> List (Html msg)
navEltIf a lst navMsg navTxt navAlign =
    if List.isEmpty lst then
        []

    else
        [ navElement a navMsg navTxt navAlign ]


viewImg : AnchorFunction msg -> msg -> TouchMsgs msg -> (ProgressiveImageMsg -> msg) -> FullImagePageModel -> Html msg
viewImg a clickMsg touchMsgs wrapProgMsg fullImagePageModel =
    let
        img =
            fullImagePageModel.album.imageFirst

        ( w, h ) =
            fitImage
                img.srcSetFirst
                (floor fullImagePageModel.viewport.viewport.width)
            <|
                Basics.round (fullImagePageModel.viewport.viewport.height * (1 - imgTitleHeight / 100))

        edgeBehaviour =
            case fullImagePageModel.prevImgs of
                [] ->
                    case fullImagePageModel.album.imageRest of
                        [] ->
                            BothLimit

                        _ ->
                            LeftLimit

                _ ->
                    case fullImagePageModel.album.imageRest of
                        [] ->
                            RightLimit

                        _ ->
                            NeitherLimit
    in
    a clickMsg
        (offsetStyles edgeBehaviour fullImagePageModel.imgPosition fullImagePageModel.offset
            ++ [ Html.Styled.Attributes.fromUnstyled <| onTouch "start" touchMsgs.touchStartMsg
               , Html.Styled.Attributes.fromUnstyled <| onTouch "move" touchMsgs.touchContinueMsg
               , Html.Styled.Attributes.fromUnstyled <| onTouch "end" touchMsgs.touchPrevNextMsg
               , id theImageId
               ]
        )
        [ Html.Styled.map wrapProgMsg <| ProgressiveImage.view <| withWidthHeight w h fullImagePageModel.progImgModel ]


offsetStyles : SwipeEdgeBehaviour -> Maybe Element -> Offset -> List (Html.Styled.Attribute msg)
offsetStyles edgeBehaviour imgPosition offset =
    let
        posStyle =
            case offset of
                NoOffset ->
                    []

                Swipe distance _ ->
                    -- note: no up/down movement is desired, just left/right
                    -- , top <| px <| Tuple.second fullImagePageModel.offset
                    [ left <| px <| elasticDistance edgeBehaviour distance ]

                Zoom (ZoomOffset z) ->
                    case imgPosition of
                        Nothing ->
                            --UGH TODO
                            []

                        Just imgPos ->
                            let
                                ( imgVpPosX, imgVpPosY ) =
                                    ( imgPos.element.x - imgPos.viewport.x
                                    , imgPos.element.y - imgPos.viewport.y
                                    )

                                ( offsetPosX, offsetPosY ) =
                                    applyOffset (ZoomOffset z) ( imgVpPosX, imgVpPosY )

                                ( deltaPosX, deltaPosY ) =
                                    ( offsetPosX - imgVpPosX, offsetPosY - imgVpPosY )

                                sc =
                                    cumScale z
                            in
                            [ --top <| px o.deltaPosY
                              --, left <| px o.deltaPosX
                              transforms [ translate2 (px deltaPosX) (px deltaPosY), scale sc ]
                            , Css.property "transform-origin" "top left"
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
