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
    = Showing String
    | Shown String
    | Hiding String
    | Hidden String


type Update
    = StartShow
    | EndShow
    | StartHide
    | EndHide


main : Program Never Model Update
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init =
    (Showing "DSC_4816.JPG", Cmd.none)


view model =
    let show url = div
         []
         [ Html.text "shown"
         , viewImg url 1 StartHide
         ]
        hide url = div
            []
          <|
            List.reverse
                [ Html.text "hidden"
                , viewImg url 0 StartShow
                ]
    in
      case model of
        Shown url ->
            show url
        Showing url ->
            show url

        Hiding url ->
            hide url
        Hidden url ->
            hide url


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


update : Update -> Model -> (Model, Cmd Update)
update msg model =
    let
        hiding =
            case model of
                Hiding _ ->
                    model

                Hidden _ ->
                    model

                Shown url ->
                    Hiding url

                Showing url ->
                    Hiding url
        showing =
            case model of
                Hiding url ->
                    Showing url

                Hidden url ->
                    Showing url

                Showing _ ->
                    model

                Shown _ ->
                    model
    in
            case msg of
                StartShow -> (showing, perform (\x -> x) (succeed EndShow))
                EndShow -> (showing, Cmd.none)
                StartHide -> (hiding, perform (\x -> x) (succeed EndHide))
                EndHide -> (hiding, Cmd.none)
