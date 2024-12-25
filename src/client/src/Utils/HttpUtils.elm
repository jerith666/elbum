module Utils.HttpUtils exposing (PercentEncoded, appendPath, encodeImgUrl, getFragment, parentUrlPath, parsePath, pctString, percentDecode, percentEncode, viewProgress)

import Album exposing (ImgSrc)
import Http exposing (..)
import List exposing (reverse)
import Parser exposing ((|.), (|=), DeadEnd, Trailing(..), chompWhile, end, getChompedString, oneOf, run, sequence, succeed)
import String exposing (endsWith, join, split)
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


type PercentEncoded
    = PercentEncoded String String


percentEncode : String -> PercentEncoded
percentEncode s =
    PercentEncoded (Url.percentEncode s) s


percentDecode : PercentEncoded -> String
percentDecode (PercentEncoded s def) =
    Maybe.withDefault def <| Url.percentDecode s


pctString : PercentEncoded -> String
pctString (PercentEncoded s _) =
    s


{-| splits the path on "/"s, calls encodeUri on each path segment, then reassembles it.
-}
encodeImgUrl : ImgSrc -> List PercentEncoded
encodeImgUrl =
    .url >> String.split "/" >> List.map percentEncode


appendPath : Url -> List PercentEncoded -> Url
appendPath baseUrl relativePath =
    let
        sep =
            case endsWith "/" baseUrl.path of
                True ->
                    ""

                False ->
                    "/"

        relPath =
            String.concat <| List.intersperse "/" <| List.map pctString relativePath

        newPath =
            baseUrl.path ++ sep ++ relPath
    in
    { baseUrl | path = newPath, query = Nothing, fragment = Nothing }


parentUrlPath : Url -> Maybe ( PercentEncoded, Url )
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
            Just ( PercentEncoded first first, { url | path = "/" ++ (join "/" <| reverse rest), query = Nothing, fragment = Nothing } )


getFragment : Url -> Result (List DeadEnd) String
getFragment url =
    case url.fragment of
        Nothing ->
            Err []

        Just f ->
            Ok f


{-| since Urls are percent-encoded already, we don't do any real conversion on the path
segments, just wrap them in PercentEncoded
-}
parsePath : (Url -> Result (List DeadEnd) String) -> Url -> Result (List DeadEnd) (List PercentEncoded)
parsePath pathGetter url =
    let
        pathParser =
            oneOf
                [ succeed [] |. end
                , succeed identity
                    |= sequence
                        { start = ""
                        , separator = "/"
                        , end = ""
                        , spaces = succeed ()
                        , item =
                            getChompedString <|
                                succeed ()
                                    |. chompWhile (\c -> c /= '/')
                        , trailing = Optional
                        }
                    |. end
                ]
    in
    case pathGetter url of
        Err errs ->
            Err errs

        Ok path ->
            Result.map (List.map (\s -> PercentEncoded s s)) <|
                Result.map (List.filter (not << String.isEmpty)) <|
                    run pathParser path
