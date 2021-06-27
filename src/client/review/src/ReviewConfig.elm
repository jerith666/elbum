module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoDuplicatePorts
import NoExposingEverything
import NoImportingEverything
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoRecursiveUpdate
import NoRedundantConcat
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
import NoUselessSubscriptions
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

        --
        , NoDuplicatePorts.rule
        , NoUnsafePorts.rule NoUnsafePorts.any
        , NoUnusedPorts.rule

        --
        , NoExposingEverything.rule
        , NoMissingTypeAnnotation.rule

        --, NoImportingEverything.rule [] -- 114 errors in 20 files; dubious benefit given IDEs
        --, NoMissingTypeAnnotationInLetIn.rule -- 166 errors in 19 files; would be quite a bit of work to fix
        --, NoMissingTypeExpose.rule -- not important for applications, only libraries
        --
        , NoMissingSubscriptionsCall.rule
        , NoRecursiveUpdate.rule
        , NoUselessSubscriptions.rule

        --
        , NoRedundantConcat.rule
        ]
