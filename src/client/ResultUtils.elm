module ResultUtils exposing (either)

import Result exposing (..)


either : (a -> c) -> (b -> c) -> Result a b -> c
either errMapper okMapper r =
    case r of
        Ok value ->
            okMapper value

        Err error ->
            errMapper error
