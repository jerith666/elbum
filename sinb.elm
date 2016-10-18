module Main exposing (..)

import Basics.Extra exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App exposing (..)
import Css exposing (..)
-- import Css.Mixin exposing (..)
-- import Css.Preprocess.Mixin exposing (..)
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


styles =
    Css.asPairs >> Html.Attributes.style

black = rgb 0 0 0
white = rgb 255 255 255


view : Model -> Html Msg
view model =
    case model.album of
        Nothing ->
            div [] [ Html.text "album loading ..." ]

        Just a ->
            div
                [ styles
                    [ Css.height (pct 100)
                    , backgroundColor black
                    , displayFlex
                    , flexDirection column
                    , paddingTop (px 1)
                    ]
                ]
                [ h1 [ styles [ color white, textAlign center ] ] [ Html.text a.title ]
                , renderImgs a.images model.index model.winWidth model.winHeight
                ]


renderImgs : List Image -> Int -> Int -> Int -> Html Msg
renderImgs imgs index winWidth winHeight =
    div [ styles [ displayFlex
                 , flexDirection column
                 , alignItems center
                 ]
        ]
        [ renderMainImage (head (drop index imgs)) winWidth winHeight
        , div
            [ styles
                [ displayFlex
                , Css.property "justify-content" "space-around"
                , alignItems center
                , overflow auto
                ]
            ]
            (renderThumbs imgs winWidth winHeight index)
        ]


renderMainImage : Maybe Image -> Int -> Int -> Html Msg
renderMainImage img winWidth winHeight =
    case img of
        Nothing ->
            div [] [ Html.text "no images in album" ]

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
                [ display block
                , margin auto
                , Css.width (pct 100)
                , Css.height (pct 100)
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
            render (iScale (fit 0.15 is1 winWidth winHeight) is1)
                   [ Css.width (pct 100), Css.height (pct 100) ]
                   Prev


iScale : Float -> ImgSrc -> ImgSrc
iScale s i =
    { i
        | x = scale i.x s
        , y = scale i.y s
    }


scale x s =
    round (toFloat x * s)


render : ImgSrc -> List Mixin -> Msg -> Html Msg
render i s msg =
    img
        [ styles s
        , Html.Attributes.src i.url
        , onClick msg
        ]
        []
