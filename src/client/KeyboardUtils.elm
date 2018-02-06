module KeyboardUtils exposing (onEscape)

import Keyboard exposing (downs)


onEscape : msg -> msg -> Sub msg
onEscape action noop =
    downs
        (\keycode ->
            case keycode of
                27 ->
                    --escape
                    action

                _ ->
                    noop
        )
