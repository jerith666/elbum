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

type alias Model = { title: String
                   , index: Int
                   , images: List Image
                   }

update msg model = model

init = { title = "The Album Title"
       , index = 1
       , images = [ [ { url = "http://mchenryfamily.org/pics/2016/2016/08_August%2024-30%3A%20Eleanor%27s%2012th%20Week/DSC_7944_Med.jpg"
                      , x = 960
                      , y = 638
                      }
                    ]
                  , [ { url = "http://mchenryfamily.org/pics/2016/2016/08_August%2024-30%3A%20Eleanor%27s%2012th%20Week/DSC_7949_Med.jpg"
                      , x = 960
                      , y = 638
                      }
                    ]
                  ]
       }

view model = div []
                 [ h1 [] [ text model.title ]
                 , renderImgs model.images model.index ]

renderImgs : List Image -> Int -> Html Model
renderImgs imgs index = case imgs of
                             [] -> div [] []
                             i::is -> if index == 0 then renderImg i else renderImgs is (index-1)

renderImg : Image -> Html Model
renderImg ises = case ises of
                      [] -> div [] []
                      is1::_ -> render is1

render i = img [ src i.url
               , width i.x
               , height i.y] []