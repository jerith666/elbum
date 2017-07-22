module AnimationTest exposing (..)

import WinSize exposing (..)
import Album exposing (..)
import AlbumPage exposing (..)
import AlbumTreeNodePage exposing (..)
import ListUtils exposing (..)
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Window exposing (..)
import Set exposing (..)
import Css exposing (..)


type Model
    = Shown String
    | Hidden String


type Update
    = Show
    | Hide


main : Program Never Model Update
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }


init =
    Shown "DSC_4816.JPG"


view model =
    case model of
        Shown url ->
            viewImg url 1 Hide

        Hidden url ->
            viewImg url 0 Show


styles =
    Css.asPairs >> Html.Attributes.style


viewImg : String -> Float -> Update -> Html Update
viewImg url op upd =
    img
        ([ Html.Attributes.src url
         , Html.Attributes.width 100
         , Html.Attributes.height 100
         , onClick upd
         ]
            ++ [ styles
                    [ opacity (num op)
                    , Css.property "transition-property" "opacity"
                    , Css.property "transition-duration" "2s"
                    , Css.property "transition-timing-function" "ease-in-out"
                    , Css.property "transition-delay" "0s"
                    ]
               ]
        )
    <|
        []


update : Update -> Model -> Model
update msg model =
    case msg of
        Hide ->
            case model of
                Hidden _ ->
                    model

                Shown url ->
                    Hidden url

        Show ->
            case model of
                Hidden url ->
                    Shown url

                Shown _ ->
                    model
