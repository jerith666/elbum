module Utils.KeyboardUtils exposing (onEscape)

import Browser.Events exposing (onKeyDown)
import Json.Decode exposing (..)


onEscape : msg -> msg -> Sub msg
onEscape action noop =
    onKeyDown <|
        map
            (\k ->
                case k of
                    "Escape" ->
                        action

                    _ ->
                        noop
            )
        <|
            field
                "key"
                string
