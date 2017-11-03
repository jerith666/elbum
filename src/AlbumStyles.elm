module AlbumStyles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Css exposing (..)


black =
    rgb 0 0 0


white =
    rgb 255 255 255


styles =
    Css.asPairs >> Html.Attributes.style


rootDivId =
    "rootDiv"


rootDiv extraStyles =
    div
        [ styles <|
            [ position absolute
            , Css.height (vh 100)
            , Css.width (vw 100)
            , overflowX Css.hidden
            , overflowY auto
            , backgroundColor black
            ]
                ++ extraStyles
        , id rootDivId
        ]


rootDivFlex dir extraStyles =
    rootDiv <|
        [ displayFlex
        , flexDirection dir
        ]
            ++ extraStyles
