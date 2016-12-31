import Album exposing (..)
import WinSize exposing (..)

import Html exposing (..)

type alias FullImagePageModel =
    { album : Album
    , index : Int
    }

view : msg -> msg -> msg -> FullImagePageModel -> Html prevNextMsg
view prevMsg nextMsg backToThumbsMsg fullImagePageModel =
    div [] [ Html.text ("Full Image Page for " ++ fullImagePageModel.album.title ++ " image " ++ fullImagePageModel.index) ]

