module Utils.HttpUtils exposing (appendPath, parentUrlPath, viewProgress)

import Http exposing (..)
import List exposing (reverse)
import String exposing (endsWith, join, split, startsWith)
import Url exposing (Url)


viewProgress : String -> Maybe Progress -> String
viewProgress prefix mProgress =
    let
        pct num denom =
            (String.fromInt <| Basics.round <| 100 * toFloat num / toFloat denom) ++ "%"
    in
    case mProgress of
        Nothing ->
            prefix

        Just progress ->
            case progress of
                Sending s ->
                    prefix ++ ": sent " ++ pct s.sent s.size

                Receiving r ->
                    case r.size of
                        Nothing ->
                            prefix ++ ": " ++ String.fromInt r.received ++ " bytes received"

                        Just size ->
                            prefix ++ ": received " ++ pct r.received size


appendPath : Url -> String -> Url
appendPath baseUrl relativePath =
    let
        sep =
            case endsWith "/" baseUrl.path || startsWith "/" relativePath of
                True ->
                    ""

                False ->
                    "/"

        newPath =
            baseUrl.path ++ sep ++ relativePath
    in
    { baseUrl | path = newPath, query = Nothing, fragment = Nothing }


parentUrlPath : Url -> Maybe ( String, Url )
parentUrlPath url =
    let
        pathSegments =
            List.filter ((/=) "") <| split "/" url.path
    in
    case reverse pathSegments of
        [ "", "" ] ->
            -- result of split "/" "/"
            Nothing

        [ "" ] ->
            Nothing

        [] ->
            Nothing

        first :: rest ->
            Just ( first, { url | path = "/" ++ (join "/" <| reverse rest), query = Nothing, fragment = Nothing } )
