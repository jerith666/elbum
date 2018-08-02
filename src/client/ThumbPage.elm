module ThumbPage exposing (ThumbPageModel, albumParent, albumTitle, colsWidth, sizeForHeight, sizeForWidth, thumbStyles, urlsToGet, view, viewThumb)

import Album exposing (..)
import AlbumStyles exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (..)
import ImageViews exposing (..)
import ListUtils exposing (..)
import Set exposing (..)
import WinSize exposing (..)


type alias ThumbPageModel =
    { album : Album
    , parents : List AlbumList
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


grey : Color
grey =
    rgb 128 128 128


view : (Float -> msg) -> (List Image -> Image -> List Image -> msg) -> (AlbumList -> msg) -> ThumbPageModel -> AlbumBootstrapFlags -> Html msg
view scrollMsgMaker imgChosenMsgr showList thumbPageModel flags =
    rootDivFlex
        flags
        column
        scrollMsgMaker
        [ overflowX Css.hidden ]
    <|
        [ albumTitle thumbPageModel.album.title thumbPageModel.parents showList [] [ position fixed ]
        , albumTitle thumbPageModel.album.title thumbPageModel.parents showList [] [ Css.property "visibility" "hidden" ]
        , div
            [ styles
                [ displayFlex
                , flexDirection row

                -- ensure the main body of the page doesn't get squished when the images overflow the viewport
                , flexShrink <| num 0
                ]
            ]
            (viewThumbs imgChosenMsgr thumbPageModel)
        ]


albumTitle : String -> List AlbumList -> (AlbumList -> msg) -> List (Html msg) -> List Style -> Html msg
albumTitle title parents showList extraHtml extraStyles =
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
        List.map (albumParent getAlbumListTitle showList) (List.reverse parents)
            ++ extraHtml
            ++ [ span [] [ Html.Styled.text title ] ]


getAlbumListTitle : AlbumList -> String
getAlbumListTitle a =
    a.listTitle


albumParent : (a -> String) -> (a -> msg) -> a -> Html msg
albumParent getTitle showList albumList =
    span []
        [ span
            [ onClick <| showList albumList
            , styles [ textDecoration underline, cursor pointer ]
            ]
            [ Html.Styled.text <| getTitle albumList ]
        , span
            [ styles [ padding2 (Css.em 0) (Css.em 0.5) ] ]
            [ Html.Styled.text "<" ]
        ]


urlsToGet : ThumbPageModel -> Set String
urlsToGet thumbPageModel =
    let
        ( _, thumbWidth ) =
            colsWidth thumbPageModel.winSize

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
            colsWidth thumbPageModel.winSize

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


colsWidth : WinSize -> ( Int, Int )
colsWidth winSize =
    let
        maxCols =
            Debug.log "maxCols" <| Basics.max (winSize.width // maxThumbWidth) 2

        thumbWidth =
            Debug.log "thumbWidth" <| (winSize.width - scrollPad) // maxCols
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
                            Partial ( 99, Nothing )
                        else
                            Completed
                in
                viewThumb thumbWidth opacity [] (imgChosenMsgr i) img
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


viewThumb : Int -> ImgLoadState -> List Style -> msg -> Image -> Html msg
viewThumb width opasity extraStyles selectedMsg img =
    let
        ( xScaled, yScaled ) =
            sizeForWidth width img
    in
    renderPresized 10
        xScaled
        yScaled
        img.srcSetFirst
        img.srcSetRest
        (thumbStyles
            ++ opacityStyles opasity
            ++ extraStyles
        )
        []
    <|
        Just selectedMsg


thumbStyles : List Style
thumbStyles =
    [ borderRadius (px 5)
    , boxShadow4 (px 1) (px 1) (px 2) (rgb 80 80 80)
    , cursor pointer
    ]


stubThumb : Int -> Image -> Html msg
stubThumb width img =
    let
        ( xScaled, yScaled ) =
            sizeForWidth width img
    in
    div
        [ styles
            [ Css.width <| px <| toFloat xScaled
            , Css.height <| px <| toFloat yScaled
            , color white
            , textAlign center
            , displayFlex
            , property "justify-content" "center"
            , property "align-content" "center"
            , flexDirection column
            , borderStyle solid
            , borderColor grey

            --TODO use one borderWidth prop
            , borderTopWidth (px 1)
            , borderBottomWidth (px 1)
            , borderLeftWidth (px 1)
            , borderRightWidth (px 1)
            , margin (px -1)
            ]
        ]
        [ Html.Styled.text "..." ]


sizeForWidth : Int -> Image -> ( Int, Int )
sizeForWidth width =
    sizeForScaler <| \is1 -> toFloat width / toFloat is1.x


sizeForHeight : Int -> Image -> ( Int, Int )
sizeForHeight height =
    sizeForScaler <| \is1 -> toFloat height / toFloat is1.y


sizeForScaler : (ImgSrc -> Float) -> Image -> ( Int, Int )
sizeForScaler scaler img =
    let
        is1 =
            img.srcSetFirst
    in
    let
        scale =
            scaler is1

        xScaled =
            Basics.round <| scale * toFloat is1.x

        yScaled =
            Basics.round <| scale * toFloat is1.y
    in
    ( xScaled, yScaled )
