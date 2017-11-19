module AnimationTest exposing (..)

import Album exposing (..)
import AlbumPage exposing (..)
import AlbumTreeNodePage exposing (..)
import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import ListUtils exposing (..)
import Set exposing (..)
import Task exposing (..)
import WinSize exposing (..)
import Window exposing (..)


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
    ( Showing "DSC_4816.JPG", Cmd.none )


view model =
    let
        show url op txt =
            div
                []
                [ Html.text txt
                , viewImg url op StartHide
                ]

        hide url op txt =
            div
                []
            <|
                List.reverse
                    [ Html.text txt
                    , viewImg url op StartShow
                    ]
    in
    case model of
        Showing url ->
            show url 0 "show0"

        Shown url ->
            show url 1 "show1"

        Hiding url ->
            hide url 1 "hide1"

        Hidden url ->
            hide url 0 "hide0"


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


update : Update -> Model -> ( Model, Cmd Update )
update msg model =
    case model of
        Showing url ->
            case msg of
                StartShow ->
                    ( model, Cmd.none )

                EndShow ->
                    ( Shown url, Cmd.none )

                StartHide ->
                    ( Hiding url, perform (\x -> x) (succeed EndHide) )

                EndHide ->
                    ( Hidden url, Cmd.none )

        Shown url ->
            case msg of
                StartShow ->
                    ( Showing url, perform (\x -> x) (succeed EndShow) )

                EndShow ->
                    ( model, Cmd.none )

                StartHide ->
                    ( Hiding url, perform (\x -> x) (succeed EndHide) )

                EndHide ->
                    ( Hidden url, Cmd.none )

        Hiding url ->
            case msg of
                StartShow ->
                    ( Showing url, perform (\x -> x) (succeed EndShow) )

                EndShow ->
                    ( Shown url, Cmd.none )

                StartHide ->
                    ( model, Cmd.none )

                EndHide ->
                    ( Hidden url, Cmd.none )

        Hidden url ->
            case msg of
                StartShow ->
                    ( Showing url, perform (\x -> x) (succeed EndShow) )

                EndShow ->
                    ( Shown url, Cmd.none )

                StartHide ->
                    ( Hiding url, perform (\x -> x) (succeed EndHide) )

                EndHide ->
                    ( model, Cmd.none )



-- Shown url ->
{-
   let hiding = case model of
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
-}
