module Utils.LocationUtils exposing (AnchorFunction, parseOriginRelativeUrl)

import Html.Styled exposing (Attribute, Html)
import Url exposing (..)


type alias AnchorFunction msg =
    msg -> List (Attribute msg) -> List (Html msg) -> Html msg


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
