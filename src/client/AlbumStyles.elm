module AlbumStyles exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)


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


opacityStyles : ( Float, Bool ) -> List Style
opacityStyles ( op, anim ) =
    case anim of
        True ->
            [ opacity (num op)
            , Css.property "transition-property" "opacity"
            , Css.property "transition-duration" "1s"
            , Css.property "transition-timing-function" "ease-in-out"
            , Css.property "transition-delay" "0s"
            ]

        False ->
            [ opacity (num op) ]
