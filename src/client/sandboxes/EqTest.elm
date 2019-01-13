module EqTest exposing (main)

import Html exposing (..)


oneThing =
    True


someThings n =
    List.repeat n oneThing


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
            case Debug.log ("testing " ++ l) <| someThings i == someThings j of
                True ->
                    l ++ ": True"

                False ->
                    l ++ ": False"

        rs =
            List.map test cp
    in
    div [] <| List.intersperse (br [] []) <| List.map text rs
