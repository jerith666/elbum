module HttpUtils exposing (getUrl, viewProgress)

import Http exposing (..)
import ListUtils exposing (..)


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


getUrl : (Result Http.Error () -> msg) -> String -> Cmd msg
getUrl handler url =
    Http.get
        { url = encodePath url
        , expect = expectWhatever handler
        }
