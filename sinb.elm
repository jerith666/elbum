import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (..)

-- MODEL

main = beginnerProgram { model = init, view = view, update = update }

type alias ImageSize = { url: String
                       , x: Int
                       , y: Int
                       }

type alias Image = List ImageSize

type alias Model = List Image

update msg model = model

init = [[{ url = "http://mchenryfamily.org/montage-2016.jpg"
         , x = 123
         , y = 456 }]]

view model = div []
                 [ div [] [ text "hello world" ]
                 , renderImgs model ]

renderImgs : List Image -> Html Model
renderImgs imgs = case imgs of
                       [] -> div [] []
                       i::_ -> renderImg i

renderImg : Image -> Html Model
renderImg ises = case ises of
                      [] -> div [] []
                      is1::_ -> render is1

render i = img [src i.url] []