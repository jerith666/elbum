import List exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App exposing (..)

-- MODEL

main = beginnerProgram { model = init, view = view, update = update }

type Msg = Next

type alias ImageSize = { url: String
                       , x: Int
                       , y: Int
                       }

type alias Image = List ImageSize

type alias Model = { title: String
                   , index: Int
                   , images: List Image
                   }

update msg model = case msg of
                        Next -> { model | index = model.index + 1 }

init = { title = "The Album Title"
       , index = 0
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
renderImg ises = case ises of
                      [] -> div [] []
                      is1::_ -> render is1

renderThumb : Image -> Html Msg
renderThumb ises = case ises of
                        [] -> div [] []
                        is1::_ -> render ( thumbScale is1 )

thumbScale : ImageSize -> ImageSize
thumbScale i = { i | x = tScale i.x
                   , y = tScale i.y
                   }

tScale x = round (toFloat x * 0.2)

render : ImageSize -> Html Msg
render i = img [ src i.url
               , width i.x
               , height i.y
               , onClick Next ]
               []