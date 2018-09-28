module LocationUtils exposing (locToString, parseHref, parseQuery)

--import Combine exposing (..)

import Http exposing (..)



--import Navigation exposing (..)


parseHref : String -> Result (ParseErr ()) (ParseOk () (List String))
parseHref href =
    let
        pathParser =
            or
                (end $> [])
            <|
                Combine.map
                    (List.filterMap identity)
                <|
                    string "#"
                        *> sepBy
                            (string "/")
                            (Combine.map decodeUri <| regex "[^/]*")
                        <* end
    in
    parse pathParser href


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
    parse qParser query


locToString : Location -> String
locToString loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname ++ loc.search ++ loc.hash
