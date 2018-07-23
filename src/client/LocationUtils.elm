module LocationUtils exposing (locToString, parseHref, parseQuery)

import Combine exposing (..)
import Http exposing (..)
import Navigation exposing (..)


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


parseQuery : String -> Result (ParseErr ()) (ParseOk () (List String))
parseQuery query =
    xxxx


locToString : Location -> String
locToString loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname ++ loc.search ++ loc.hash
