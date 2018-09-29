module LocationUtils exposing (locToString, parseHref, parseQuery)

import Browser.Navigation exposing (..)
import Http exposing (..)
import Parser exposing (..)
import Url exposing (..)


parseHref : String -> Result (ParseErr ()) (ParseOk () (List String))
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


parseQuery : String -> Result (ParseErr ()) (ParseOk () (Maybe String))
parseQuery query =
    let
        rest =
            while (\_ -> True) <* end

        qParser =
            or
                (end $> Nothing)
            <|
                Combine.map Just <|
                    string "?s="
                        *> rest
    in
    run qParser query


locToString : Location -> String
locToString loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname ++ loc.search ++ loc.hash
