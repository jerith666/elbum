module Utils.ResultUtils exposing (either, toCmd)

import Result exposing (..)
import Task


either : (a -> c) -> (b -> c) -> Result a b -> c
either errMapper okMapper r =
    case r of
        Ok value ->
            okMapper value

        Err error ->
            errMapper error


toCmd : a -> Cmd a
toCmd m =
    Task.perform identity <| Task.succeed m
