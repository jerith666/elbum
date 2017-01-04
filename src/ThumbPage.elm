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

maxThumbWidth = 400

view : (Int -> msg) -> ThumbPageModel -> Html msg
view imgChosenMsgr thumbPageModel =
    rootDivFlexRow
        [ backgroundColor black ]
        <| viewThumbs imgChosenMsgr thumbPageModel


viewThumbs : (Int -> msg) -> ThumbPageModel -> List (Html msg)
viewThumbs imgChosenMsgr thumbPageModel =
    List.map (viewThumbColumn imgChosenMsgr)
    <| spreadThumbs thumbPageModel.winSize.width maxThumbWidth thumbPageModel.album.images []


viewThumbColumn : (Int -> msg) -> List (Image,Int) -> Html msg
viewThumbColumn imgChosenMsgr images =
    let
        viewThumbTuple (img,i) = viewThumb (imgChosenMsgr i) img
    in
        div
            [ styles
                [ displayFlex
                , flexDirection column
                ]
            ]
            <| List.map viewThumbTuple images

spreadThumbs : Int -> Int -> List Image -> List (List (Image,Int)) -> List (List (Image,Int))
spreadThumbs spanWidth maxImgWidth images alreadySpreadImages =
    case List.head images of
        Just nextImg ->
            insertImage spanWidth maxImgWidth (List.sum <| List.map List.length alreadySpreadImages) nextImg alreadySpreadImages
            |> spreadThumbs spanWidth maxImgWidth (List.drop 1 images)
        Nothing ->
            alreadySpreadImages

insertImage : Int -> Int -> Int -> Image -> List (List (Image,Int)) -> List (List (Image,Int))
insertImage spanWidth maxImgWidth i nextImg alreadySpreadImages =
    let
        col = 1 + List.length alreadySpreadImages
    in
        if col * maxImgWidth <= spanWidth then
            alreadySpreadImages ++ [[(
            Debug.log ("start column " ++ (toString col) ++ " with image " ++ (toString i))
            nextImg
            , i)]]
        else
            let
                is = findShortest alreadySpreadImages
                iShortest = Debug.log ("image " ++ (toString i) ++ " goes in (col,height) ") is
            in
                mapI (Tuple.first iShortest) (\x -> x ++ [(nextImg, i)]) alreadySpreadImages


shorterBaseCase : (Int, Int)
shorterBaseCase = (0,999999)

findShortest : List (List (Image, Int)) -> (Int,Int)
findShortest imageLists =
    List.foldr
        shorter
        shorterBaseCase
        (List.indexedMap (,) (List.map (List.sum << (List.map (imgHeight << Tuple.first))) imageLists))

--TODO would be nice to have maxBy, minBy
shorter : (Int,Int) -> (Int,Int) -> (Int,Int)
shorter (i1, h1) (i2, h2) =
    if h1 <= h2 then
        (i1, h1)
    else
        (i2, h2)

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
            viewThumb (imgChosenMsgr i) nextImg
            :: viewThumbs2 (i+1) imgChosenMsgr thumbPageModel

        Nothing ->
            []


viewThumb : msg -> Image -> Html msg
viewThumb selectedMsg img =
    case img.srcSet of
        [] ->
             div [] []

        is1 :: _ ->
            render is1 img.srcSet [] selectedMsg

--

render : ImgSrc -> List ImgSrc -> List Mixin -> msg -> Html msg
render idefault is s msg =
    img
        [ styles s
        , Html.Attributes.src idefault.url
        , attribute "srcset" (encodeSrcSet is)
        , Html.Attributes.width idefault.x
        , Html.Attributes.height idefault.y
        , onClick msg
        ]
        []


encodeSrcSet : List ImgSrc -> String
encodeSrcSet is =
  String.join ", " (List.map encodeSrc is)


encodeSrc : ImgSrc -> String
encodeSrc is =
  is.url ++ " " ++ (toString is.x) ++ "w"


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

rootDivFlexRow extraStyles =
    rootDiv <|
        [ displayFlex
        , flexDirection row
        ]
        ++ extraStyles

--

mapI : Int -> (a -> a) -> List a -> List a
mapI i map l =
    let
        ifmap (j, a) = if i == j then (map a) else a
    in
        List.map ifmap <| List.indexedMap (,) l

-- div [onClick (imgChosenMsgr 1)] [ Html.text ("Thumb Page for " ++ thumbPageModel.album.title ++ " at " ++ (toString thumbPageModel.winSize.width) ++ "x" ++ (toString thumbPageModel.winSize.height)) ]

