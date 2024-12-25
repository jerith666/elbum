module HttpUtils exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Url exposing (Protocol(..), Url)
import Utils.HttpUtils exposing (parentUrlPath, percentDecode, percentEncode)


exampleDotComPath : String -> Url
exampleDotComPath path =
    { protocol = Https
    , host = "example.com"
    , port_ = Nothing
    , path = path
    , query = Nothing
    , fragment = Nothing
    }


suite : Test
suite =
    describe "parseOriginRelativeUrl"
        [ test "empty path returns nothing" <|
            \_ ->
                Expect.equal Nothing <|
                    parentUrlPath <|
                        exampleDotComPath ""
        , test "root path returns nothing" <|
            \_ ->
                Expect.equal Nothing <|
                    parentUrlPath <|
                        exampleDotComPath "/"
        , test "one path returns no path" <|
            \_ ->
                Expect.equal (Just <| ( percentEncode "foo", exampleDotComPath "/" )) <|
                    parentUrlPath <|
                        exampleDotComPath "/foo"
        , test "two paths returns one path" <|
            \_ ->
                Expect.equal (Just <| ( percentEncode "bar", exampleDotComPath "/foo" )) <|
                    parentUrlPath <|
                        exampleDotComPath "/foo/bar"
        , test "two paths with trailing slash returns one path" <|
            \_ ->
                Expect.equal (Just <| ( percentEncode "bar", exampleDotComPath "/foo" )) <|
                    parentUrlPath <|
                        exampleDotComPath "/foo/bar/"
        , test "two paths with query and fragment drops query and fragment" <|
            \_ ->
                let
                    x =
                        exampleDotComPath "/foo/bar"

                    y =
                        { x | query = Just "q", fragment = Just "f" }
                in
                Expect.equal (Just <| ( percentEncode "bar", exampleDotComPath "/foo" )) <|
                    parentUrlPath y
        , test "two paths with spaces returns one path, percent-encoded" <|
            \_ ->
                Expect.equal (Just <| ( "baz quux", exampleDotComPath "/foo%20bar" )) <|
                    Maybe.map (Tuple.mapFirst percentDecode) <|
                        parentUrlPath <|
                            exampleDotComPath "/foo%20bar/baz%20quux"
        ]
