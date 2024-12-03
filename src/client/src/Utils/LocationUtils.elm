module Utils.LocationUtils exposing (AnchorFunction, parseOriginRelativeUrl, parsePath)

import Html.Styled exposing (Attribute, Html)
import Parser exposing (..)
import Url exposing (..)


type alias AnchorFunction msg =
    msg -> List (Attribute msg) -> List (Html msg) -> Html msg


parsePath : String -> Result (List DeadEnd) (List String)
parsePath path =
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
                            map (\p -> Maybe.withDefault p <| percentDecode p) <|
                                getChompedString <|
                                    succeed ()
                                        |. chompWhile (\c -> c /= '/')
                        , trailing = Optional
                        }
                    |. end
                ]
    in
    Result.map (List.filter (not << String.isEmpty)) <|
        run pathParser path


{-| Url.fromString doesn't support origin-relative URLs:
Url.fromString "/" == Nothing
so we implement it ourselves
-}
parseOriginRelativeUrl : Url -> String -> Maybe Url
parseOriginRelativeUrl baseUrl urlStr =
    case Url.fromString urlStr of
        Just url ->
            Just url

        Nothing ->
            case String.startsWith "/" urlStr of
                True ->
                    -- make up a URL that fromString will like
                    case Url.fromString <| "https://example.com" ++ urlStr of
                        Just url ->
                            -- if it worked, smash it together with baseUrl
                            Just { baseUrl | path = url.path, query = url.query, fragment = url.fragment }

                        Nothing ->
                            Nothing

                False ->
                    Nothing
