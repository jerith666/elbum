module ThumbPage exposing (ThumbPageModel, view)

import Album exposing (..)
import WinSize exposing (..)
import ImageViews exposing (..)
import AlbumStyles exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Css exposing (..)


type alias ThumbPageModel =
    { album : Album
    , parent: Maybe AlbumTreeNode
    , winSize : WinSize
    }


maxThumbWidth =
    300


scrollPad =
    20


view : (List Image -> Image -> List Image -> msg) -> ThumbPageModel -> Html msg
view imgChosenMsgr thumbPageModel =
    rootDivFlex column
        [ overflowX Css.hidden ]
    <|
        [ albumTitle thumbPageModel.album.title [ position fixed ]
        , albumTitle thumbPageModel.album.title [ Css.property "visibility" "hidden" ]
        , div
            [ styles
                [ displayFlex
                , flexDirection row
                ]
            ]
            (viewThumbs imgChosenMsgr thumbPageModel)
        ]


albumTitle : String -> List Mixin -> Html msg
albumTitle title extraStyles =
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
        [ Html.text title ]


viewThumbs : (List Image -> Image -> List Image -> msg) -> ThumbPageModel -> List (Html msg)
viewThumbs imgChosenMsgr thumbPageModel =
    let
        maxCols =
            Debug.log "maxCols" <| Basics.max (thumbPageModel.winSize.width // maxThumbWidth) 2

        thumbWidth =
            Debug.log "thumbWidth" <| (thumbPageModel.winSize.width - scrollPad) // maxCols

        imgs =
            thumbPageModel.album.imageFirst :: thumbPageModel.album.imageRest
    in
        List.map (viewThumbColumn thumbWidth (convertImgChosenMsgr thumbPageModel.album.imageFirst imgs imgChosenMsgr)) <|
            spreadThumbs maxCols imgs []


convertImgChosenMsgr : Image -> List Image -> (List Image -> Image -> List Image -> msg) -> (Int -> msg)
convertImgChosenMsgr image1 images prevCurRestImgChosenMsgr =
    \i ->
        let
            prev =
                Debug.log ("prev i=" ++ (toString i)) (List.take i images)

            cur =
                case List.head (List.drop i images) of
                    Just img ->
                        Debug.log ("cur i=" ++ (toString i)) img

                    Nothing ->
                        Debug.log ("cur NOTHING!!! i=" ++ (toString i)) image1

            next =
                Debug.log ("next i=" ++ (toString i)) (List.drop (i + 1) images)
        in
            prevCurRestImgChosenMsgr prev cur next


viewThumbColumn : Int -> (Int -> msg) -> List ( Image, Int ) -> Html msg
viewThumbColumn thumbWidth imgChosenMsgr images =
    let
        viewThumbTuple ( img, i ) =
            viewThumb thumbWidth (imgChosenMsgr i) img
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
            ++ [ [ ( Debug.log ("start column " ++ (toString (List.length alreadySpreadImages)) ++ " with image " ++ (toString i))
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
                Debug.log ("image " ++ (toString i) ++ " goes in (col,height) ") is
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
        (Debug.log "heights" (List.indexedMap (,) (List.map (List.sum << (List.map (imgHeight << Tuple.first))) imageLists)))



--TODO would be nice to have maxBy, minBy


shorter : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
shorter ( i1, h1 ) ( i2, h2 ) =
    if h1 <= h2 then
        ( i1, h1 )
    else
        ( i2, h2 )


imgHeight img =
    let
        is1 =
            img.srcSetFirst
    in
        Basics.round <| (toFloat is1.y) * (1000 / toFloat is1.x)


viewThumb : Int -> msg -> Image -> Html msg
viewThumb width selectedMsg img =
    let
        is1 =
            img.srcSetFirst
    in
        let
            scale =
                (toFloat width) / (toFloat is1.x)

            xScaled =
                Basics.round <| scale * (toFloat is1.x)

            yScaled =
                Basics.round <| scale * (toFloat is1.y)
        in
            renderPresized 10
                xScaled
                yScaled
                (img.srcSetFirst :: img.srcSetRest)
                [ borderRadius (px 5)
                , boxShadow4 (px 1) (px 1) (px 2) (rgb 80 80 80)
                ]
                []
                selectedMsg



-- TODO move below to utils file


mapI : Int -> (a -> a) -> List a -> List a
mapI i map l =
    let
        ifmap ( j, a ) =
            if i == j then
                (map a)
            else
                a
    in
        List.map ifmap <| List.indexedMap (,) l
