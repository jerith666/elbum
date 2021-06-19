module Utils.LocationUtils exposing (AnchorFunction, changeToString, locToString, parseHash)

import Html.Styled exposing (Attribute, Html)
import Parser exposing (..)
import RouteUrl exposing (UrlChange(..))
import Url exposing (..)


type alias AnchorFunction msg =
    msg -> List (Attribute msg) -> List (Html msg) -> Html msg


parseHash : String -> Result (List DeadEnd) (List String)
parseHash href =
    let
        hashParser =
            oneOf
                [ succeed [] |. end
                , succeed identity
                    |= sequence
                        { start = ""
                        , separator = "/"
                        , end = ""
                        , spaces = succeed ()
                        , item =
                            map (\p -> Maybe.withDefault p <| percentDecode p) <|
                                getChompedString <|
                                    succeed ()
                                        |. chompWhile (\c -> c /= '/')
                        , trailing = Optional
                        }
                    |. end
                ]
    in
    run hashParser href


locToString : Url -> String
locToString =
    toString


changeToString : UrlChange -> String
changeToString change =
    case change of
        NewPath _ data ->
            data.path
                |> addPrefixed "?" data.query
                |> addPrefixed "#" data.fragment

        NewQuery _ data ->
            "?"
                ++ data.query
                |> addPrefixed "#" data.fragment

        NewFragment _ fragment ->
            "#" ++ fragment


addPrefixed : String -> Maybe String -> String -> String
addPrefixed prefix maybeSegment starter =
    case maybeSegment of
        Nothing ->
            starter

        Just segment ->
            starter ++ prefix ++ segment
