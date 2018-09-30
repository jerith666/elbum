module LocationUtils exposing (locToString, parseHref, parseQuery)

import Browser.Navigation exposing (..)
import Http exposing (..)
import Parser exposing (..)
import Url exposing (..)


parseHref : String -> Result (List DeadEnd) (List String)
parseHref href =
    let
        pathParser =
            oneOf
                [ succeed [] |. end
                , succeed identity
                    |= sequence
                        { start = "#"
                        , separator = "/"
                        , end = ""
                        , spaces = end
                        , item = map percentEncode <| getChompedString <| succeed () |. chompWhile (\_ -> True)
                        , trailing = Optional
                        }
                    |. end
                ]
    in
    run pathParser href


parseQuery : String -> Result (List DeadEnd) (Maybe String)
parseQuery query =
    let
        qParser =
            oneOf
                [ succeed Nothing |. end
                , succeed Just
                    |. symbol "?s="
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
