module LocationUtils exposing (locToString, parseHref)

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


locToString : Location -> String
locToString loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname ++ loc.search ++ loc.hash
