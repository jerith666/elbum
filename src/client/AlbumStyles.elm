module AlbumStyles exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (..)


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


type alias AlbumBootstrapFlags =
    { scrollSupport : Bool
    }


type ImgLoadState
    = Requested
    | Partial ( Int, Maybe Int ) --bytes loaded, maybe total bytes
      -- | Aborted
      -- | Failed
    | Completed
    | Shown --not really a state, a bit of a hack
    | Disappearing --not really a state, a bit of a hack


{-| this prevents "bouncy" scrolling on iOS, as recommended at
<https://stackoverflow.com/a/29629214/47552>. using position:fixed on
chrome prevents scrolling the address bar off the top of the screen,
sacrificing valuable screen real estate, so we have to avoid it
there.
-}
rootPos : AlbumBootstrapFlags -> Style
rootPos flags =
    if flags.scrollSupport then
        position fixed
    else
        position absolute


rootDiv : AlbumBootstrapFlags -> List Style -> List (Html msg) -> Html msg
rootDiv flags extraStyles =
    div
        [ styles <|
            [ rootPos flags
            , Css.height (vh 100)
            , Css.width (vw 100)
            , overflowX Css.hidden
            , overflowY auto
            , backgroundColor black
            ]
                ++ extraStyles
        , Html.Styled.Attributes.id rootDivId
        ]


rootDivFlex : AlbumBootstrapFlags -> FlexDirection compatible -> List Style -> List (Html msg) -> Html msg
rootDivFlex flags dir extraStyles =
    rootDiv flags <|
        [ displayFlex
        , flexDirection dir
        ]
            ++ extraStyles


opacityStyles : ImgLoadState -> List Style
opacityStyles imgLoadedState =
    case imgLoadedState of
        Requested ->
            [ opacity <| num 0 ]

        Partial int ->
            [ opacity <| num 0 ]

        Completed ->
            opacityAnimatedTo 1

        Shown ->
            [ opacity <| num 1 ]

        Disappearing ->
            opacityAnimatedTo 0


opacityDuration : Float
opacityDuration =
    1


opacityAnimatedTo : Float -> List Style
opacityAnimatedTo opasity =
    [ opacity (num opasity)
    , Css.property "transition-property" "opacity"
    , Css.property "transition-duration" <| toString opacityDuration ++ "s"
    , Css.property "transition-timing-function" "ease-in-out"
    , Css.property "transition-delay" "0s"
    ]


navEltSize : number
navEltSize =
    50


navBoxStyles : List Style
navBoxStyles =
    [ position absolute
    , Css.height (px navEltSize)
    , Css.width (px navEltSize)
    , lineHeight (px navEltSize)
    , textAlign center
    , color white
    , backgroundColor (rgba 40 40 40 0.5)
    , borderRadius (px <| navEltSize / 2)
    , cursor pointer
    ]


navElement : msg -> String -> (Px -> Style) -> Html msg
navElement msg label side =
    div
        [ styles <|
            navBoxStyles
                ++ [ side (px 0)
                   ]
        , onClick msg
        ]
        [ Html.Styled.text label ]
