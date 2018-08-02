module ListUtils exposing (dictWithValues, dropThrough, dropThroughPred, encodePath, fromMaybe, mapI, shiftLeft, shiftRight, shiftToBeginning)

import Dict exposing (..)
import Http exposing (encodeUri)
import Set exposing (..)


{-| splits the path on "/"s, calls encodeUri on each path segment, then reassembles it.
-}
encodePath : String -> String
encodePath =
    String.split "/" >> List.map encodeUri >> String.join "/"


dictWithValues : Set comparable -> a -> Dict comparable a
dictWithValues keys val =
    Dict.fromList <|
        List.map (\key -> ( key, val )) <|
            Set.toList keys


shiftToBeginning : List a -> a -> List a -> ( a, List a )
shiftToBeginning prevImgs img restImgs =
    case prevImgs of
        [] ->
            ( img, restImgs )

        prev1 :: prevRest ->
            ( prev1, prevRest ++ (img :: restImgs) )


{-| return a tuple with the new middle element taken from the beginning of the right hand list,
the old middle element appended to the left hand list, if possible, otherwise do nothing
-}
shiftRight : List a -> a -> List a -> ( List a, a, List a )
shiftRight xLefts x xRights =
    case xRights of
        [] ->
            ( xLefts, x, xRights )

        xRight :: xRightRights ->
            ( xLefts ++ [ x ], xRight, xRightRights )


{-| return a tuple with the new middle element taken from the end of the left hand list,
the old middle element prepended to the right hand list, if possible, otherwise do nothing
-}
shiftLeft : List a -> a -> List a -> ( List a, a, List a )
shiftLeft xLefts x xRights =
    case xLefts of
        [] ->
            ( xLefts, x, xRights )

        [ xLeft ] ->
            ( [], xLeft, x :: xRights )

        xLeft :: xLeftRights ->
            let
                ( xLRss, xss, xRss ) =
                    shiftLeft xLeftRights x xRights
            in
            ( xLeft :: xLRss, xss, xRss )


{-| drop elements of the given list until the given element is found.
if that element is not present, return the entire list.
-}
dropThrough : a -> List a -> List a
dropThrough elem elems =
    dropThroughPred (\e -> e == elem) elems


{-| drop elements of the given list until the given element is found.
if that element is not present, return the entire list.
-}
dropThroughPred : (a -> Bool) -> List a -> List a
dropThroughPred pred elems =
    case elems of
        [] ->
            []

        e :: es ->
            if pred e then
                es
            else if List.any pred elems then
                dropThroughPred pred es
            else
                elems


{-| apply the given map function to only the ith element of the given list
-}
mapI : Int -> (a -> a) -> List a -> List a
mapI i map l =
    let
        ifmap ( j, a ) =
            if i == j then
                map a
            else
                a
    in
    List.map ifmap <| List.indexedMap (,) l


fromMaybe : Maybe a -> List a
fromMaybe m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]
