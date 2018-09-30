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
                [ succeed (\_ -> []) end
                , sequence
                    { start = "#"
                    , separator = "/"
                    , end = ""
                    , spaces = end
                    , item = map percentEncode getChompedString <| succeed () |. chompWhile (\_ -> True)
                    , trailing = Optional
                    }
                ]
    in
    run pathParser href


parseQuery : String -> Result (List DeadEnd) (Maybe String)
parseQuery query =
    let
        qParser =
            oneOf
                [ succeed (\_ -> Nothing) end
                , succeed identity
                    |. symbol "?s="
                    |= getChompedString
                  <|
                    succeed ()
                        |. chompWhile (\_ -> True)
                ]
    in
    run qParser query


locToString : Url -> String
locToString =
    toString
