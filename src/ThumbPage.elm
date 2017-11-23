module ThumbPage exposing (ThumbPageModel, albumTitle, urlsToGet, view, viewThumb)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import ImageViews exposing (..)
import ListUtils exposing (..)
import Set exposing (..)
import WinSize exposing (..)


type alias ThumbPageModel =
    { album : Album
    , parents : List AlbumTreeNode
    , winSize : WinSize
    , justLoadedImages : Set String
    , readyToDisplayImages : Set String
    }


maxThumbWidth : Int
maxThumbWidth =
    300


scrollPad : Int
scrollPad =
    20


view : (List Image -> Image -> List Image -> msg) -> (AlbumTreeNode -> msg) -> ThumbPageModel -> Html msg
view imgChosenMsgr showNode thumbPageModel =
    rootDivFlex column
        [ overflowX Css.hidden ]
    <|
        [ albumTitle thumbPageModel.album.title thumbPageModel.parents showNode [ position fixed ]
        , albumTitle thumbPageModel.album.title thumbPageModel.parents showNode [ Css.property "visibility" "hidden" ]
        , div
            [ styles
                [ displayFlex
                , flexDirection row
                ]
            ]
            (viewThumbs imgChosenMsgr thumbPageModel)
        ]


albumTitle : String -> List AlbumTreeNode -> (AlbumTreeNode -> msg) -> List Mixin -> Html msg
albumTitle title parents showNode extraStyles =
    div
        [ styles <|
            [ color white
            , textAlign center
            , Css.width (vw 100)
            , backgroundColor (rgba 40 40 40 0.5)
            , padding (px 5)
            ]
                ++ extraStyles
        ]
    <|
        List.map (albumParent showNode) (List.reverse parents)
            ++ [ span [] [ Html.text title ] ]


albumParent : (AlbumTreeNode -> msg) -> AlbumTreeNode -> Html msg
albumParent showNode albumTreeNode =
    span
        [ onClick <| showNode albumTreeNode ]
        [ Html.text <| albumTreeNode.nodeTitle ++ " < " ]


urlsToGet : ThumbPageModel -> Set String
urlsToGet thumbPageModel =
    let
        ( _, thumbWidth ) =
            colsWidth thumbPageModel

        srcs =
            List.map (srcForWidth thumbWidth) <| thumbPageModel.album.imageFirst :: thumbPageModel.album.imageRest
    in
    Debug.log "urlsToGet" <|
        Set.fromList <|
            List.take 5 <|
                List.filter
                    (\url -> not <| member url <| Set.union thumbPageModel.justLoadedImages thumbPageModel.readyToDisplayImages)
                <|
                    List.map
                        (\i -> i.url)
                        srcs


viewThumbs : (List Image -> Image -> List Image -> msg) -> ThumbPageModel -> List (Html msg)
viewThumbs imgChosenMsgr thumbPageModel =
    let
        ( maxCols, thumbWidth ) =
            colsWidth thumbPageModel

        imgs =
            thumbPageModel.album.imageFirst :: thumbPageModel.album.imageRest
    in
    List.map
        (viewThumbColumn thumbWidth
            (convertImgChosenMsgr thumbPageModel.album.imageFirst imgs imgChosenMsgr)
            thumbPageModel.justLoadedImages
            thumbPageModel.readyToDisplayImages
        )
    <|
        spreadThumbs maxCols imgs []


colsWidth : ThumbPageModel -> ( Int, Int )
colsWidth thumbPageModel =
    let
        maxCols =
            Debug.log "maxCols" <| Basics.max (thumbPageModel.winSize.width // maxThumbWidth) 2

        thumbWidth =
            Debug.log "thumbWidth" <| (thumbPageModel.winSize.width - scrollPad) // maxCols
    in
    ( maxCols, thumbWidth )


convertImgChosenMsgr : Image -> List Image -> (List Image -> Image -> List Image -> msg) -> (Int -> msg)
convertImgChosenMsgr image1 images prevCurRestImgChosenMsgr =
    \i ->
        let
            prev =
                Debug.log ("prev i=" ++ toString i) (List.take i images)

            cur =
                case List.head (List.drop i images) of
                    Just img ->
                        Debug.log ("cur i=" ++ toString i) img

                    Nothing ->
                        Debug.log ("cur NOTHING!!! i=" ++ toString i) image1

            next =
                Debug.log ("next i=" ++ toString i) (List.drop (i + 1) images)
        in
        prevCurRestImgChosenMsgr prev cur next


viewThumbColumn : Int -> (Int -> msg) -> Set String -> Set String -> List ( Image, Int ) -> Html msg
viewThumbColumn thumbWidth imgChosenMsgr justLoadedImages readyToDisplayImages images =
    let
        viewThumbTuple ( img, i ) =
            let
                src =
                    srcForWidth thumbWidth img
            in
            if member src.url <| Set.union justLoadedImages readyToDisplayImages then
                let
                    opacity =
                        if member src.url justLoadedImages then
                            ( 0, False )
                        else
                            ( 1, True )
                in
                viewThumb thumbWidth opacity (imgChosenMsgr i) img
                --TODO opacity
            else
                stubThumb thumbWidth img
    in
    div
        [ styles
            [ displayFlex
            , flexDirection column
            ]
        ]
    <|
        List.map viewThumbTuple images


spreadThumbs : Int -> List Image -> List (List ( Image, Int )) -> List (List ( Image, Int ))
spreadThumbs maxCols images alreadySpreadImages =
    case List.head images of
        Just nextImg ->
            insertImage maxCols (List.sum <| List.map List.length alreadySpreadImages) nextImg alreadySpreadImages
                |> spreadThumbs maxCols (List.drop 1 images)

        Nothing ->
            alreadySpreadImages


insertImage : Int -> Int -> Image -> List (List ( Image, Int )) -> List (List ( Image, Int ))
insertImage maxCols i nextImg alreadySpreadImages =
    if List.length alreadySpreadImages < maxCols then
        alreadySpreadImages
            ++ [ [ ( Debug.log ("start column " ++ toString (List.length alreadySpreadImages) ++ " with image " ++ toString i)
                        nextImg
                   , i
                   )
                 ]
               ]
    else
        let
            is =
                findShortest alreadySpreadImages

            iShortest =
                Debug.log ("image " ++ toString i ++ " goes in (col,height) ") is
        in
        mapI (Tuple.first iShortest) (\x -> x ++ [ ( nextImg, i ) ]) alreadySpreadImages


shorterBaseCase : ( Int, Int )
shorterBaseCase =
    ( 0, 999999 )


findShortest : List (List ( Image, Int )) -> ( Int, Int )
findShortest imageLists =
    List.foldr
        shorter
        shorterBaseCase
        (Debug.log "heights" (List.indexedMap (,) (List.map (List.sum << List.map (imgHeight << Tuple.first)) imageLists)))



--TODO would be nice to have maxBy, minBy


shorter : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
shorter ( i1, h1 ) ( i2, h2 ) =
    if h1 <= h2 then
        ( i1, h1 )
    else
        ( i2, h2 )


imgHeight : Image -> Int
imgHeight img =
    let
        is1 =
            img.srcSetFirst
    in
    Basics.round <| toFloat is1.y * (1000 / toFloat is1.x)


srcForWidth : Int -> Image -> ImgSrc
srcForWidth width img =
    let
        ( xScaled, yScaled ) =
            sizeForWidth width img
    in
    smallestImageBiggerThan xScaled yScaled img.srcSetFirst img.srcSetRest


viewThumb : Int -> ( Float, Bool ) -> msg -> Image -> Html msg
viewThumb width opasity selectedMsg img =
    let
        ( xScaled, yScaled ) =
            sizeForWidth width img
    in
    renderPresized 10
        xScaled
        yScaled
        img.srcSetFirst
        img.srcSetRest
        ([ borderRadius (px 5)
         , boxShadow4 (px 1) (px 1) (px 2) (rgb 80 80 80)
         ]
            ++ opacityStyles opasity
        )
        []
    <|
        Just selectedMsg


opacityStyles : ( Float, Bool ) -> List Mixin
opacityStyles ( op, anim ) =
    case anim of
        True ->
            [ opacity (num op)
            , property "transition-property" "opacity"
            , property "transition-duration" "1s"
            , property "transition-timing-function" "ease-in-out"
            , property "transition-delay" "0s"
            ]

        False ->
            [ opacity (num op) ]


loadingImg : ImgSrc
loadingImg =
    { url = "loading.png"
    , x = 1
    , y = 1
    }


stubThumb : Int -> Image -> Html msg
stubThumb width img =
    let
        ( xScaled, yScaled ) =
            sizeForWidth width img
    in
    renderPresized 10
        xScaled
        yScaled
        loadingImg
        []
        []
        []
        Nothing



{- div
   [ styles
       [ Css.width <| px <| toFloat xScaled
       , height <| px <| toFloat yScaled
       , color white
       , textAlign center
       ]
   ]
   [ Html.text "..." ]
-}


sizeForWidth : Int -> Image -> ( Int, Int )
sizeForWidth width img =
    let
        is1 =
            img.srcSetFirst
    in
    let
        scale =
            toFloat width / toFloat is1.x

        xScaled =
            Basics.round <| scale * toFloat is1.x

        yScaled =
            Basics.round <| scale * toFloat is1.y
    in
    ( xScaled, yScaled )
