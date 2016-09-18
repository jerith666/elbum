import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App exposing (..)
import Keyboard exposing (..)
import Task exposing (..)
import Http exposing (..)

import Album exposing (..)

-- MODEL

main = program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

type Msg = Next
         | Prev
         | NoUpdate
         | NoAlbum Http.Error
         | YesAlbum Album

type alias Model = { album: Maybe Album
                   , index: Int
                   }

subscriptions model = downs (\keycode -> case keycode of
                                              39 -> Next {- right arrow -}
                                              37 -> Prev {- left arrow -}
                                              _ -> NoUpdate)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
       NoAlbum err -> ( model
                      , Cmd.none
                      )
       YesAlbum album -> ( { model
                           | album = Just album
                           }
                         , Cmd.none
                         )
       Next -> moveindex model (\i -> i + 1)
       Prev -> moveindex model (\i -> i - 1)
       NoUpdate -> moveindex model (\i -> i)

moveindex model mover =
  ( case model.album of
         Nothing -> model
         Just a -> let newi = mover model.index
                   in { model
                      | index = Basics.min (Basics.max newi
                                                       0)
                                           (length a.images - 1)
                      }
  , Cmd.none
  )

init = ( { album = Nothing
         , index = 0
         }
       , Task.perform NoAlbum YesAlbum (Http.get jsonDecAlbum "album.json")
       )

view : Model -> Html Msg
view model =
  case model.album of
       Nothing -> div [] [ text "album loading ..." ]
       Just a ->
         div []
             [ h1 [] [ text a.title ]
             , renderImgs a.images model.index
             ]

renderImgs : List Image -> Int -> Html Msg
renderImgs imgs index =
  div []
      ( [ renderMainImage ( head ( drop index imgs ) ) ]
        ++ [ br [] [] ] ++
        renderThumbs imgs index
      )

renderMainImage : Maybe Image -> Html Msg
renderMainImage img =
  case img of
       Nothing -> div [] [text "no images in album"]
       Just i -> renderImg i

renderThumbs imgs index =
  List.map renderThumb imgs

renderImg : Image -> Html Msg
renderImg ises = case ises.srcSet.srcs of
                      [] -> div [] []
                      is1::_ -> render is1 Next

renderThumb : Image -> Html Msg
renderThumb ises = case ises.srcSet.srcs of
                        [] -> div [] []
                        is1::_ -> render ( thumbScale is1 ) Prev

thumbScale : ImgSrc -> ImgSrc
thumbScale i = { i | x = tScale i.x
                   , y = tScale i.y
                   }

tScale x = round (toFloat x * 0.2)

render : ImgSrc -> Msg -> Html Msg
render i msg = img [ src i.url
                   , width i.x
                   , height i.y
                   , onClick msg ]
                   []