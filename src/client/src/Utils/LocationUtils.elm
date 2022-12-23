module Utils.LocationUtils exposing (AnchorFunction, parseHash)

import Html.Styled exposing (Attribute, Html)
import Parser exposing (..)
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
