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
    rootDivFlexCol
        [ backgroundColor black ]
        <| viewThumbs 0 imgChosenMsgr thumbPageModel


spreadThumbs : Int -> Int -> List Image -> List (List Image) -> List (List Image)
spreadThumbs spanWidth maxImgWidth images alreadySpreadImages =
    case List.head images of
        Just nextImg ->
            insertImage spanWidth maxImgWidth nextImg alreadySpreadImages
            |> spreadThumbs spanWidth maxImgWidth (List.drop 1 images)
        Nothing ->
            alreadySpreadImages

insertImage : Int -> Int -> Image -> List (List Image) -> List (List Image)
insertImage spanWidth maxImgWidth nextImg alreadySpreadImages =
    if (1 + List.length alreadySpreadImages) * maxImgWidth <= spanWidth then
        alreadySpreadImages ++ [[nextImg]]
    else
        let
            iShortest = findShortest alreadySpreadImages
        in
            mapI (Tuple.second iShortest) (\x -> x ++ [nextImg]) alreadySpreadImages


shorterBaseCase : (Int, Int)
shorterBaseCase = (0,0)


findShortest : List (List Image) -> (Int,Int)
findShortest imageLists =
    List.foldr
        shorter
        shorterBaseCase
        (List.indexedMap (,) (List.map (List.sum << (List.map imgHeight)) imageLists))

--TODO would be nice to have maxBy, minBy
shorter : (Int,Int) -> (Int,Int) -> (Int,Int)
shorter (i1, h1) (i2, h2) =
    if h1 <= h2 then
        (i1, h1)
    else
        (i2, h2)

imgHeight img =
    case img.srcSet of
        [] -> 0
        is1 :: _ -> is1.y

viewThumbs : Int -> (Int -> msg) -> ThumbPageModel -> List (Html msg)
viewThumbs i imgChosenMsgr thumbPageModel =
    case List.head <| List.drop i thumbPageModel.album.images of
        Just nextImg ->
            viewThumb (imgChosenMsgr i) nextImg
            :: viewThumbs (i+1) imgChosenMsgr thumbPageModel

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

rootDivFlexCol extraStyles =
    rootDiv <|
        [ displayFlex
        , flexDirection column
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

