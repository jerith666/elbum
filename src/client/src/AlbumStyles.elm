module AlbumStyles exposing (ImgLoadState(..), MainAlbumFlags, black, navBoxStyles, navElement, navEltSize, opacityStyles, rootDivFlex, rootDivId, rootPos, styles, theImageId, white)

import Browser.Dom exposing (..)
import Css exposing (..)
import Css.Transitions exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)
import Json.Decode exposing (..)
import Utils.LocationUtils exposing (AnchorFunction)


black : Color
black =
    rgb 0 0 0


white : Color
white =
    rgb 255 255 255


styles : List Style -> Attribute msg
styles =
    css


rootDivId : String
rootDivId =
    "rootDiv"


theImageId : String
theImageId =
    "the-image"


type alias MainAlbumFlags =
    { scrollSupport : Bool
    }


type ImgLoadState
    = ImgFetched
    | ImgLoaded


{-| this prevents "bouncy" scrolling on iOS, as recommended at
<https://stackoverflow.com/a/29629214/47552>. using position:fixed on
chrome prevents scrolling the address bar off the top of the screen,
sacrificing valuable screen real estate, so we have to avoid it
there.
-}
rootPos : MainAlbumFlags -> Style
rootPos flags =
    if flags.scrollSupport then
        position fixed

    else
        position absolute


rootDiv : MainAlbumFlags -> Maybe (Viewport -> msg) -> Viewport -> List Style -> List (Html msg) -> Html msg
rootDiv flags scrollMsgMaker viewport extraStyles =
    div <|
        [ styles <|
            [ rootPos flags
            , Css.height (px viewport.viewport.height)
            , Css.width (vw 100)
            , overflowX Css.hidden
            , overflowY auto
            , Css.property "-webkit-overflow-scrolling" "touch"
            , Css.backgroundColor black
            ]
                ++ extraStyles
        , Html.Styled.Attributes.id rootDivId
        ]
            ++ (case scrollMsgMaker of
                    Nothing ->
                        []

                    Just sMM ->
                        [ on "scroll" <| Json.Decode.map sMM <| Json.Decode.at [ "target" ] viewportDecoder ]
               )


viewportDecoder : Decoder Viewport
viewportDecoder =
    map6
        (\width ->
            \height ->
                \x ->
                    \y ->
                        \vwidth ->
                            \vheight ->
                                { scene =
                                    { width = width
                                    , height = height
                                    }
                                , viewport =
                                    { x = x
                                    , y = y
                                    , width = vwidth
                                    , height = vheight
                                    }
                                }
        )
        (field "scrollWidth" Json.Decode.float)
        (field "scrollHeight" Json.Decode.float)
        (field "scrollLeft" Json.Decode.float)
        (field "scrollTop" Json.Decode.float)
        (field "clientWidth" Json.Decode.float)
        (field "clientHeight" Json.Decode.float)


rootDivFlex : MainAlbumFlags -> FlexDirection compatible -> Maybe (Viewport -> msg) -> Viewport -> List Style -> List (Html msg) -> Html msg
rootDivFlex flags dir scrollMsgMaker viewport extraStyles =
    rootDiv flags scrollMsgMaker viewport <|
        [ displayFlex
        , flexDirection dir
        ]
            ++ extraStyles


opacityStyles : ImgLoadState -> List Style
opacityStyles imgLoadedState =
    case imgLoadedState of
        ImgFetched ->
            [ Css.opacity <| num 0 ]

        ImgLoaded ->
            opacityAnimatedTo 1


opacityDurationMillis : Float
opacityDurationMillis =
    1000


opacityAnimatedTo : Float -> List Style
opacityAnimatedTo opasity =
    [ Css.opacity (num opasity)
    , transition
        [ Css.Transitions.opacity3
            opacityDurationMillis
            0
            easeInOut
        ]
    ]


navEltSize : number
navEltSize =
    50


navBoxStyles : List Style
navBoxStyles =
    [ position absolute
    , Css.height (px navEltSize)
    , Css.width (px navEltSize)
    , Css.lineHeight (px navEltSize)
    , textAlign center
    , Css.color white
    , Css.backgroundColor (rgba 40 40 40 0.5)
    , Css.borderRadius (px <| navEltSize / 2)
    , textDecoration none
    ]


navElement : AnchorFunction msg -> msg -> String -> (Px -> Style) -> Html msg
navElement a msg label side =
    a msg
        [ styles <|
            navBoxStyles
                ++ [ side (px 0)
                   ]
        ]
        [ Html.Styled.text label ]
