module ResultUtils exposing (either, serialize, toCmd)

import Result exposing (..)
import Task exposing (..)


either : (a -> c) -> (b -> c) -> Result a b -> c
either errMapper okMapper r =
    case r of
        Ok value ->
            okMapper value

        Err error ->
            errMapper error


serialize : List a -> (a -> a -> a) -> a -> Cmd a
serialize msgs reducer id =
    Task.perform identity <|
        Task.map (List.foldl reducer id) <|
            s2 msgs


s2 : List a -> Task Never (List a)
s2 msgs =
    Task.sequence <| s1 msgs


s1 : List a -> List (Task Never a)
s1 msgs =
    List.map (\m -> Task.succeed m) msgs


toCmd : a -> Cmd a
toCmd m =
    Task.perform identity <| Task.succeed m
