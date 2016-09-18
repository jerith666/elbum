import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App exposing (..)
import Keyboard exposing (..)

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

type alias Model = { album: Maybe Album
                   , index: Int
                   }

subscriptions model = downs (\keycode -> case keycode of
                                              39 -> Next {- right arrow -}
                                              37 -> Prev {- left arrow -}
                                              _ -> NoUpdate)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case model.album of
       Nothing -> ( model
                  , Cmd.none
                  )
       Just a ->
         ( case msg of
                Next -> { model
                        | index = Basics.min (model.index + 1)
                                             (length a.images - 1) }
                Prev -> { model
                        | index = Basics.max (model.index - 1)
                                             0 }
                NoUpdate -> model
         , Cmd.none
         )

init = ( { album = Nothing
         , index = 0
         }
       , Cmd.none
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
renderImgs imgs index = case imgs of
                             [] -> div [] []
                             i::is -> if index == 0 then
                                          div [] ( [ renderImg i ]
                                                 ++ (List.map renderThumb is)
                                                 )
                                      else
                                          div [] [ renderThumb i
                                                 , renderImgs is (index-1)
                                                 ]

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