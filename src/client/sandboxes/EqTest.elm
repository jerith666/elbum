module EqTest exposing (main)

import Html exposing (..)


oneSrc =
    { url = "a url", x = 1, y = 2 }


oneImg =
    { altText = "an image", srcSetFirst = oneSrc, srcSetRest = [ oneSrc ] }


someImgs n =
    List.repeat n oneImg


main =
    let
        r =
            List.range 100 101

        cp =
            List.concatMap (\i -> List.map (\j -> ( i, j )) r) r

        test ( i, j ) =
            let
                l =
                    String.fromInt i ++ " ?= " ++ String.fromInt j
            in
            case Debug.log ("testing " ++ l) <| someImgs i == someImgs j of
                True ->
                    l ++ ": True"

                False ->
                    l ++ ": False"

        rs =
            List.map test cp
    in
    div [] <| List.intersperse (br [] []) <| List.map text rs


type alias Image =
    { altText : String
    , srcSetFirst : ImgSrc
    , srcSetRest : List ImgSrc
    }


type alias ImgSrc =
    { url : String
    , x : Int
    , y : Int
    }
