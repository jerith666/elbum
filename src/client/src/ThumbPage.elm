module ThumbPage exposing (ThumbPageModel, albumParent, albumTitle, allUrls, colsWidth, sizeForHeight, sizeForWidth, thumbStyles, urlsToGet, view)

import Album exposing (..)
import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (on)
import Http exposing (Progress(..))
import ImageViews exposing (..)
import Json.Decode as Decoder
import Progress.Ring
import String exposing (fromInt)
import Url exposing (Url)
import Utils.HttpUtils exposing (appendPath)
import Utils.ListUtils exposing (..)
import Utils.Loading exposing (LoadState(..), ManyModel, getOneState)
import Utils.LocationUtils exposing (AnchorFunction)


{-| This module renders the thumbnail view of an album of images.

Another interesting part is how we ensure that each thumbnail
nicely fades in only once it is completely loaded, while also
giving the user some indication that thumbnails are loading. This
is hard because <img/> elements don't provide onProgress events,
only onLoad events.

After trying several work-arounds, what I've settled on that seems to
work the best is this: We fetch the image's Url using Http.get. And
then we know (okay, technically, "can assume/hope/bet") that the
contents of that URL will remain cached while we add an <img/> element
to the DOM.

That <img/>'s onLoad event will still fire -- usually almost
instantly, but if the preceding assumption fails, it'll fire after the
browser has re-downloaded the image.

At that point we can change the style from `opacity: 0` to `opacity:
1; transition opacity NNN 0s easeInOut`. This is important because
CSS transitions only render if the DOM element was already present
_without_ the transition.

The code in this module delegates the progress tracking to the
`Loading` module. Once we see that the image's url is loaded, we
create the <img/> element and attach an `onLoad` listener to it. When
that fires, we use `Loading.markOne` to indicate that it's time to
fade in the immage, and then the `Marked` state tells us to re-render
the <img/> element with the transition.

Another important consideration, since `Url`s are being built somewhat
dynamically, is to ensure that the `Url` used with the `Loading`
module is the same as that used with the <img/> element -- otherwise,
the assumption that the <img/> element will find the image data
already in the cache won't hold. (The two cases this bit me were
percent-encoding and extra slashes.)

-}
type alias ThumbPageModel msg =
    { album : Album
    , parents : List AlbumList
    , bodyViewport : Viewport
    , rootDivViewport : Maybe Viewport
    , imageLoader : ManyModel msg
    , baseUrl : Url
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


view : AnchorFunction msg -> (Viewport -> msg) -> (List Image -> Image -> List Image -> msg) -> (Url -> msg) -> (AlbumList -> msg) -> ThumbPageModel msgB -> MainAlbumFlags -> Html msg
view a scrollMsgMaker imgChosenMsgr loadedMsg showList thumbPageModel flags =
    rootDivFlex
        flags
        column
        (Just scrollMsgMaker)
        thumbPageModel.bodyViewport
        [ overflowX Css.hidden ]
    <|
        [ albumTitle a thumbPageModel.album.title thumbPageModel.parents showList [] [ position fixed ]
        , albumTitle a thumbPageModel.album.title thumbPageModel.parents showList [] [ visibility hidden ]
        , div
            [ styles
                [ displayFlex
                , flexDirection row

                -- ensure the main body of the page doesn't get squished when the images overflow the viewport
                , flexShrink <| num 0
                ]
            ]
            (viewThumbs a imgChosenMsgr loadedMsg thumbPageModel)
        ]


albumTitle : AnchorFunction msg -> String -> List AlbumList -> (AlbumList -> msg) -> List (Html msg) -> List Style -> Html msg
albumTitle a title parents showList extraHtml extraStyles =
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
        List.map (albumParent a getAlbumListTitle showList) (List.reverse parents)
            ++ extraHtml
            ++ [ span [] [ Html.Styled.text title ] ]


getAlbumListTitle : AlbumList -> String
getAlbumListTitle a =
    a.listTitle


albumParent : AnchorFunction msg -> (a -> String) -> (a -> msg) -> a -> Html msg
albumParent a getTitle showList albumList =
    span []
        [ a (showList albumList)
            [ styles [ textDecoration underline, color inherit ] ]
            [ Html.Styled.text <| getTitle albumList ]
        , span
            [ styles [ padding2 (Css.em 0) (Css.em 0.5) ] ]
            [ Html.Styled.text "<" ]
        ]


allUrls : Url -> ThumbPageModel msg -> List Url
allUrls baseUrl =
    allImgSrcs
        >> List.map .url
        >> List.map (appendPath baseUrl)


allImgSrcs : ThumbPageModel msg -> List ImgSrc
allImgSrcs thumbPageModel =
    let
        ( _, thumbWidth ) =
            colsWidth thumbPageModel.bodyViewport
    in
    List.map (srcForWidth thumbWidth) <| thumbPageModel.album.imageFirst :: thumbPageModel.album.imageRest


urlsToGet : ThumbPageModel msg -> List Url
urlsToGet thumbPageModel =
    let
        srcs =
            allImgSrcs thumbPageModel

        vPort =
            thumbPageModel.rootDivViewport

        scrollPct =
            Maybe.withDefault 0 <|
                Maybe.map
                    (\vp ->
                        case vp.viewport.y == 0 of
                            True ->
                                0

                            False ->
                                (vp.viewport.y + vp.viewport.height / 2)
                                    / vp.scene.height
                    )
                    vPort

        score i =
            let
                iPct =
                    toFloat i / (toFloat <| List.length srcs)
            in
            abs (scrollPct - iPct)

        scoredSrcs =
            List.indexedMap (\i -> \img -> ( score i, img )) srcs

        prioritySrcs =
            List.map Tuple.second <| List.sortBy Tuple.first scoredSrcs
    in
    List.filter
        (\url ->
            not <|
                case getOneState thumbPageModel.imageLoader url of
                    Just Loaded ->
                        True

                    Just (Marked Loaded) ->
                        True

                    Just (Failed _) ->
                        True

                    Just (Marked (Failed _)) ->
                        True

                    _ ->
                        False
        )
    <|
        List.map (.url >> encodePath >> appendPath thumbPageModel.baseUrl) prioritySrcs


viewThumbs : AnchorFunction msg -> (List Image -> Image -> List Image -> msg) -> (Url -> msg) -> ThumbPageModel msgB -> List (Html msg)
viewThumbs a imgChosenMsgr loadedMsg thumbPageModel =
    let
        ( maxCols, thumbWidth ) =
            colsWidth thumbPageModel.bodyViewport

        imgs =
            thumbPageModel.album.imageFirst :: thumbPageModel.album.imageRest
    in
    List.map
        (viewThumbColumn a
            thumbWidth
            (convertImgChosenMsgr thumbPageModel.album.imageFirst imgs imgChosenMsgr)
            loadedMsg
            thumbPageModel.imageLoader
            thumbPageModel.baseUrl
        )
    <|
        spreadThumbs maxCols imgs []


colsWidth : Viewport -> ( Int, Int )
colsWidth viewport =
    let
        maxCols =
            Basics.max (floor viewport.viewport.width // maxThumbWidth) 2

        thumbWidth =
            (floor viewport.viewport.width - scrollPad) // maxCols
    in
    ( maxCols, thumbWidth )


convertImgChosenMsgr : Image -> List Image -> (List Image -> Image -> List Image -> msg) -> (Int -> msg)
convertImgChosenMsgr image1 images prevCurRestImgChosenMsgr =
    \i ->
        let
            prev =
                List.take i images

            cur =
                case List.head (List.drop i images) of
                    Just img ->
                        img

                    Nothing ->
                        image1

            next =
                List.drop (i + 1) images
        in
        prevCurRestImgChosenMsgr prev cur next


viewThumbColumn : AnchorFunction msg -> Int -> (Int -> msg) -> (Url -> msg) -> ManyModel msgB -> Url -> List ( Image, Int ) -> Html msg
viewThumbColumn a thumbWidth imgChosenMsgr loadedMsg imageLoader baseUrl images =
    let
        viewThumbTuple ( img, i ) =
            let
                src =
                    srcForWidth thumbWidth img

                srcUrl =
                    appendPath baseUrl <| encodePath src.url

                loadState =
                    getOneState imageLoader srcUrl

                srcLoadState =
                    case loadState of
                        Just Loaded ->
                            Just ImgFetched

                        Just (Marked Loaded) ->
                            Just ImgLoaded

                        _ ->
                            Nothing
            in
            case srcLoadState of
                Just opacity ->
                    viewThumb a thumbWidth opacity [] (imgChosenMsgr i) (loadedMsg srcUrl) img

                Nothing ->
                    stubThumb thumbWidth img loadState
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
            insertImage
                maxCols
                (List.sum <| List.map List.length alreadySpreadImages)
                nextImg
                alreadySpreadImages
                |> spreadThumbs
                    maxCols
                    (List.drop 1 images)

        Nothing ->
            alreadySpreadImages


insertImage : Int -> Int -> Image -> List (List ( Image, Int )) -> List (List ( Image, Int ))
insertImage maxCols i nextImg alreadySpreadImages =
    if List.length alreadySpreadImages < maxCols then
        alreadySpreadImages
            ++ [ [ ( nextImg, i ) ] ]

    else
        let
            is =
                findShortest alreadySpreadImages

            iShortest =
                is
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
        (List.indexedMap
            (\a b -> ( a, b ))
            (List.map (List.sum << List.map (imgHeight << Tuple.first)) imageLists)
        )



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


viewThumb : AnchorFunction msg -> Int -> ImgLoadState -> List Style -> msg -> msg -> Image -> Html msg
viewThumb a width opasity extraStyles selectedMsg loadedMsg img =
    let
        ( xScaled, yScaled ) =
            sizeForWidth width img

        onLoadAttr =
            case opasity of
                ImgFetched ->
                    [ on "load" <| Decoder.succeed loadedMsg ]

                ImgLoaded ->
                    []
    in
    a selectedMsg
        []
        [ renderPresized 10
            xScaled
            yScaled
            img.srcSetFirst
            img.srcSetRest
            (thumbStyles
                ++ opacityStyles opasity
                ++ extraStyles
            )
            onLoadAttr
        ]


thumbStyles : List Style
thumbStyles =
    [ borderRadius (px 5)
    , boxShadow4 (px 1) (px 1) (px 2) (rgb 80 80 80)
    , cursor pointer
    ]


stubThumb : Int -> Image -> Maybe LoadState -> Html msg
stubThumb width img progress =
    let
        ( xScaled, yScaled ) =
            sizeForWidth width img
    in
    div
        [ styles
            [ Css.width <| px <| toFloat xScaled
            , Css.height <| px <| toFloat yScaled
            , color white
            , alignItems center
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
        [ renderProgress progress ]


renderProgress : Maybe LoadState -> Html msg
renderProgress l =
    case l of
        Just (Loading (Receiving r)) ->
            case r.size of
                Just sz ->
                    let
                        pct =
                            toFloat r.received / toFloat sz
                    in
                    fromUnstyled <|
                        Progress.Ring.view
                            { color = "#bfbfbf"
                            , progress = pct
                            , radius = 20
                            , stroke = 5
                            }

                Nothing ->
                    text <|
                        case modBy 3 r.received of
                            0 ->
                                ".  "

                            1 ->
                                " . "

                            _ ->
                                "  ."

        _ ->
            text "..."


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
