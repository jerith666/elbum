port module Scroll exposing (getScroll)


port getScroll : (Float -> msg) -> Sub msg
