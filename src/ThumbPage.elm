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
    , winSize : WinSize
    }


maxThumbWidth =
    300


scrollPad =
    20


view : (Int -> msg) -> ThumbPageModel -> Html msg
view imgChosenMsgr thumbPageModel =
    rootDivFlex column
        [ backgroundColor black ]
    <|
        albumTitle thumbPageModel.album.title [position fixed]
        ::
        albumTitle thumbPageModel.album.title [Css.property "visibility" "hidden"]
        ::
        div
            [ styles [ displayFlex
                     , flexDirection row
                     ]
            ]
            (viewThumbs imgChosenMsgr thumbPageModel)
        :: []


albumTitle : String -> List Mixin -> Html msg
albumTitle title extraStyles =
    div
        [ styles <|
            [ color white
            , textAlign center
            , Css.width (vw 100)
            , backgroundColor (rgba 40 40 40 0.5)
            ]
                ++ extraStyles
        ]
        [ Html.text title ]


viewThumbs : (Int -> msg) -> ThumbPageModel -> List (Html msg)
viewThumbs imgChosenMsgr thumbPageModel =
    let
        maxCols =
            Debug.log "maxCols" <| Basics.max (thumbPageModel.winSize.width // maxThumbWidth) 2

        thumbWidth =
            Debug.log "thumbWidth" <| (thumbPageModel.winSize.width - scrollPad) // maxCols
    in
        List.map (viewThumbColumn thumbWidth imgChosenMsgr) <|
            spreadThumbs maxCols thumbPageModel.album.images []


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
            ++ [ [ ( Debug.log ("start column " ++ (toString col) ++ " with image " ++ (toString i))
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
    case img.srcSet of
        [] ->
            0

        is1 :: _ ->
            Basics.round <| (toFloat is1.y) * (1000 / toFloat is1.x)


viewThumbs2 : Int -> (Int -> msg) -> ThumbPageModel -> List (Html msg)
viewThumbs2 i imgChosenMsgr thumbPageModel =
    case List.head <| List.drop i thumbPageModel.album.images of
        Just nextImg ->
            viewThumb 300 (imgChosenMsgr i) nextImg
                :: viewThumbs2 (i + 1) imgChosenMsgr thumbPageModel

        Nothing ->
            []


viewThumb : Int -> msg -> Image -> Html msg
viewThumb width selectedMsg img =
    case img.srcSet of
        [] ->
            div [] []

        is1 :: _ ->
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
                    img.srcSet
                    [ borderRadius (px 5)
                    , boxShadow4 (px 1) (px 1) (px 2) (rgb 80 80 80)
                    ]
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
