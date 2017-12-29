module KeyboardUtils exposing (onUpArrow)

import Keyboard exposing (downs)


onUpArrow : msg -> msg -> Sub msg
onUpArrow action noop =
    downs
        (\keycode ->
            case keycode of
                38 ->
                    --up arrow
                    action

                _ ->
                    noop
        )
