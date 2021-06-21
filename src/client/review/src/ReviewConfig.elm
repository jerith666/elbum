module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoDuplicatePorts
import NoUnsafePorts
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import Review.Rule exposing (Rule, ignoreErrorsForDirectories, ignoreErrorsForFiles)


config : List Rule
config =
    List.map
        (ignoreErrorsForDirectories [ "vendor" ]
            >> ignoreErrorsForFiles [ "src/Album.elm" ]
        )
        [ NoUnused.CustomTypeConstructors.rule []
        , NoUnused.CustomTypeConstructorArgs.rule
            |> ignoreErrorsForFiles [ "src/Utils/Loading.elm" ]
        , NoUnused.Dependencies.rule
        , NoUnused.Exports.rule
            |> ignoreErrorsForFiles [ "src/Utils/Loading.elm" ]
        , NoUnused.Modules.rule
        , NoUnused.Parameters.rule
            |> ignoreErrorsForFiles [ "src/Utils/DebugSupport.elm" ]
        , NoUnused.Patterns.rule
        , NoUnused.Variables.rule
        , NoDuplicatePorts.rule
        , NoUnsafePorts.rule NoUnsafePorts.any
        , NoUnusedPorts.rule
        ]
