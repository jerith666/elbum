module Utils.LocationUtils exposing (locToString, parseHash, parseQuery)

import Browser.Navigation exposing (..)
import Http exposing (..)
import Parser exposing (..)
import Url exposing (..)


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


parseQuery : String -> Result (List DeadEnd) (Maybe String)
parseQuery query =
    let
        qParser =
            oneOf
                [ succeed Nothing |. end
                , succeed Just
                    |. symbol "s="
                    |= (getChompedString <|
                            succeed ()
                                |. chompWhile (\_ -> True)
                       )
                    |. end
                ]
    in
    run qParser query


locToString : Url -> String
locToString =
    toString
