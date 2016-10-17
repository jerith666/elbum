module Main exposing (..)

import Basics.Extra exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App exposing (..)
import Keyboard exposing (..)
import Task exposing (..)
import Http exposing (..)
import Window exposing (..)
import Album exposing (..)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Next
    | Prev
    | NoUpdate
    | NoAlbum Http.Error
    | YesAlbum Album
    | Resize Size


type alias Model =
    { album : Maybe Album
    , index : Int
    , winWidth : Int
    , winHeight : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { album = Nothing
      , index = 0
      , winWidth = 0
      , winHeight = 0
      }
    , Cmd.batch
        [ Task.perform NoAlbum YesAlbum (Http.get jsonDecAlbum "album.json")
        , Task.perform never Resize Window.size
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ downs
            (\keycode ->
                case keycode of
                    39 ->
                        -- right arrow
                        Next

                    37 ->
                        -- left arrow
                        Prev

                    _ ->
                        NoUpdate
            )
        , resizes Resize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoUpdate ->
            ( model
            , Cmd.none
            )

        NoAlbum err ->
            ( model
            , Cmd.none
            )

        YesAlbum album ->
            ( { model
                | album = Just album
              }
            , Cmd.none
            )

        Next ->
            moveindex model (\i -> i + 1)

        Prev ->
            moveindex model (\i -> i - 1)

        Resize size ->
            ( { model
                | winWidth = size.width
                , winHeight = size.height
              }
            , Cmd.none
            )


moveindex model mover =
    ( case model.album of
        Nothing ->
            model

        Just a ->
            let
                newi =
                    mover model.index
            in
                { model
                    | index =
                        Basics.min
                            (Basics.max newi
                                0
                            )
                            (length a.images - 1)
                }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model.album of
        Nothing ->
            div [] [ text "album loading ..." ]

        Just a ->
            div
                [ style
                    [ ( "height"
                      , "100%"
                      )
                    , ( "background-color"
                      , "black"
                      )
                    ]
                ]
                [ h1 [ style [ ( "color", "white" ) ] ] [ text a.title ]
                , renderImgs a.images model.index model.winWidth model.winHeight
                ]


renderImgs : List Image -> Int -> Int -> Int -> Html Msg
renderImgs imgs index winWidth winHeight =
    div []
        [ div [] [ renderMainImage (head (drop index imgs)) winWidth winHeight ]
        , div
            [ style
                [ ( "display", "flex" )
                , ( "justify-content", "space-around" )
                , ( "align-items", "center" )
                , ( "overflow", "auto" )
                ]
            ]
            (renderThumbs imgs winWidth winHeight index)
        ]


renderMainImage : Maybe Image -> Int -> Int -> Html Msg
renderMainImage img winWidth winHeight =
    case img of
        Nothing ->
            div [] [ text "no images in album" ]

        Just i ->
            renderImg i winWidth winHeight


renderThumbs imgs winWidth winHeight index =
    List.map (renderThumb winWidth winHeight) imgs


renderImg : Image -> Int -> Int -> Html Msg
renderImg ises winWidth winHeight =
    case ises.srcSet of
        [] ->
            div [] []

        is1 :: _ ->
            render (iScale (fit 0.75 is1 winWidth winHeight) is1)
                [ ( "display", "block" )
                , ( "margin", "auto" )
                ]
                Next


fit : Float -> ImgSrc -> Int -> Int -> Float
fit s i width height =
    s
        * (Basics.min ((toFloat width) / (toFloat i.x))
            ((toFloat height) / (toFloat i.y))
          )


renderThumb : Int -> Int -> Image -> Html Msg
renderThumb winWidth winHeight ises =
    case ises.srcSet of
        [] ->
            div [] []

        is1 :: _ ->
            render (iScale (fit 0.15 is1 winWidth winHeight) is1) [] Prev


iScale : Float -> ImgSrc -> ImgSrc
iScale s i =
    { i
        | x = scale i.x s
        , y = scale i.y s
    }


scale x s =
    round (toFloat x * s)


render : ImgSrc -> List ( String, String ) -> Msg -> Html Msg
render i s msg =
    img
        [ style s
        , src i.url
        , Html.Attributes.width i.x
        , Html.Attributes.height i.y
        , onClick msg
        ]
        []
