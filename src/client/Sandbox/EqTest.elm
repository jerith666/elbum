module Sandbox.EqTest exposing (main)

import Html exposing (..)


oneThing =
    True


someThings n =
    List.repeat n oneThing


main =
    let
        r =
            List.range 90 110

        cp =
            --List.filter (\( i, j ) -> i /= j) <|
            List.concatMap (\i -> List.map (\j -> ( i, j )) r) r

        test ( i, j ) =
            let
                l =
                    String.fromInt i ++ " ?= " ++ String.fromInt j
            in
            case Debug.log ("testing " ++ l) <| safeEq (someThings i) (someThings j) of
                True ->
                    l ++ ": True"

                False ->
                    l ++ ": False"

        rs =
            List.map test cp
    in
    div [] <| List.intersperse (br [] []) <| List.map text rs


safeEq : List a -> List a -> Bool
safeEq l1 l2 =
    let
        ll1 =
            List.length l1

        ll2 =
            List.length l2
    in
    case max ll1 ll2 >= 100 of
        False ->
            l1 == l2

        True ->
            (ll1 == ll2)
                && (List.all identity <| List.map2 (==) l1 l2)
