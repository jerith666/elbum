module ThumbPage exposing (ThumbPageModel, view)

import Album exposing (..)
import WinSize exposing (..)

import Html exposing (..)

type alias ThumbPageModel =
    { album : Album
    , winSize : WinSize
    }

view : (Int -> msg) -> ThumbPageModel -> Html msg
view imgChosenMsgr thumbPageModel =
    div [] [ Html.text ("Thumb Page for " ++ thumbPageModel.album.title ++ " at " ++ (toString thumbPageModel.winSize.width) ++ "x" ++ (toString thumbPageModel.winSize.height)) ]

