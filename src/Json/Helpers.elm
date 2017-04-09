module Json.Helpers
    exposing
        ( ObjectEncoding
        , encodeValue
        , encodeObject
        , decodeSumObjectWithSingleField
        , decodeSumTwoElemArray
        , decodeSumTaggedObject
        , encodeSumObjectWithSingleField
        , encodeSumTwoElementArray
        , encodeSumTaggedObject
        , decodeMap
        , encodeMap
        , jsonEncDict
        , jsonDecDict
        , encodeSet
        , decodeSet
        , decodeSumUnaries
        , maybeEncode
        )

{-| This module exposes helper functions for encoding sum types and maps. It was designed
with an eye for compatibility with the `aeson` library from the Haskell world, which explains
why the various functions have such peculiar names.

If you require Haskell interop, please take a look at the [elm-bridge](https://hackage.haskell.org/package/elm-bridge) package that
will make it easy to derive the Elm code alongside the Haskell one.

# The ObjectEncoding type
@docs ObjectEncoding, encodeObject, encodeValue

# Encoding schemes

The following Elm type will be used as an example for the different encoding schemes.

    type Foo = Bar Int
             | Baz { a: Int , b: Int }
             | Qux Int Int

## ObjectWithSingleField

    -- {"Bar":5}
    -- {"Baz":{"a":4,"b":8}}
    -- {"Qux":[98,42]}

@docs decodeSumObjectWithSingleField, encodeSumObjectWithSingleField

## TwoElemArray

    -- ["Bar",5]
    -- ["Baz",{"a":4,"b":8}]
    -- ["Qux",[98,42]]

@docs decodeSumTwoElemArray, encodeSumTwoElementArray

## TaggedObject

    -- {"tag":"Bar","content":5}
    -- {"tag":"Baz","a":4,"b":8}
    -- ["tag":"Qux","content":[98,42]}

@docs decodeSumTaggedObject, encodeSumTaggedObject

## Unary sum types

@docs decodeSumUnaries

# Containers helpers

@docs decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Encode
import Json.Decode exposing (field, Value)


{-| This is an opaque type that is to be used to give hints when using the `TaggedObject` encoding.
-}
type ObjectEncoding
    = EObject (List ( String, Value ))
    | EValue Value


{-| Creates an `ObjectEncoding`, just like the `Json.Encode.object` function.
-}
encodeObject : List ( String, Value ) -> ObjectEncoding
encodeObject =
    EObject


{-| Creates an `ObjectEncoding` from any type of `Value`. You should not use this for `Value`s that are actually objects.
-}
encodeValue : Value -> ObjectEncoding
encodeValue =
    EValue


oeValue : ObjectEncoding -> Value
oeValue x =
    case x of
        EObject o ->
            Json.Encode.object o

        EValue v ->
            v


{-| Encodes an optional value, using `null` when there is `Nothing`
-}
maybeEncode : (a -> Value) -> Maybe a -> Value
maybeEncode e v =
    case v of
        Nothing ->
            Json.Encode.null

        Just a ->
            e a


resmapM : (a -> Result r b) -> List a -> Result r (List b)
resmapM f lst =
    case lst of
        [] ->
            Ok []

        x :: xs ->
            f x |> Result.andThen (\nx -> resmapM f xs |> Result.andThen (\nxs -> Ok (nx :: nxs)))


-- polyfill
tuple2 : (a -> b -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder value
tuple2 abv da db =
    Json.Decode.map2 abv (Json.Decode.index 0 da) (Json.Decode.index 1 db)


-- polyfill from https://groups.google.com/d/msg/elm-dev/Ctl_kSKJuYc/7nCM8XETBwAJ
customDecoder decoder toResult =
    Json.Decode.andThen
        (\a ->
            case toResult a of
                Ok b -> Json.Decode.succeed b
                Err err -> Json.Decode.fail err
        )
        decoder


{-| Decode objects encoded using the `ObjectWithSingleField` scheme.
The first argument is the human readable name of the type of data, and will be used in error messages.
The second argument is a `Dict` where the keys are the tags of each constructor of the sum type and the values
are decoders for each case.
-}
decodeSumObjectWithSingleField : String -> Dict String (Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeSumObjectWithSingleField name mapping =
    customDecoder (Json.Decode.keyValuePairs Json.Decode.value)
        (\lst ->
            case lst of
                [ ( key, value ) ] ->
                    decodeSumFinal name key value mapping

                _ ->
                    Err ("Can't decode " ++ name ++ ": object expected to match pattern [ ( key, value ) ], but has too many keys: " ++ (toString lst))
        )


{-| Decode objects encoded using the `TwoElemArray` scheme.
The first argument is the human readable name of the type of data, and will be used in error messages.
The second argument is a `Dict` where the keys are the tags of each constructor of the sum type and the values
are decoders for each case.
-}
decodeSumTwoElemArray : String -> Dict String (Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeSumTwoElemArray name mapping =
    customDecoder (tuple2 (,) Json.Decode.string Json.Decode.value) (\( key, value ) -> decodeSumFinal name key value mapping)


{-| Decode objects encoded using the `TaggedObject` scheme.
The first argument is the human readable name of the type of data, and will be used in error messages.
The second argument is a `Dict` where the keys are the tags of each constructor of the sum type and the values
are decoders for each case.

Compared to the other functions, it expects a set of `String`s. This sets lists all the constructor tags that have an object content,
such as the `Baz` constructor in the example.
-}
decodeSumTaggedObject : String -> String -> String -> Dict String (Json.Decode.Decoder a) -> Set String -> Json.Decode.Decoder a
decodeSumTaggedObject name fieldname contentname mapping objectKeys =
    (field fieldname Json.Decode.string)
        |> Json.Decode.andThen
            (\key ->
                let
                    decoder =
                        if Set.member key objectKeys then
                            Json.Decode.value
                        else
                            field contentname Json.Decode.value
                in
                    customDecoder decoder (\value -> decodeSumFinal name key value mapping)
            )


decodeSumFinal : String -> String -> Value -> Dict String (Json.Decode.Decoder a) -> Result String a
decodeSumFinal name key value mapping =
    case Dict.get key mapping of
        Nothing ->
            Err ("Unknown constructor " ++ key ++ " for type " ++ name)

        Just dec ->
            Json.Decode.decodeValue dec value


{-| Encode objects using the `WithSingleField` scheme.
The first argument is a function that, for each possible value `a`, must return a `String` tag
describing it along with an `ObjectEncoding`.
-}
encodeSumObjectWithSingleField : (a -> ( String, ObjectEncoding )) -> a -> Value
encodeSumObjectWithSingleField mkkeyval v =
    let
        ( key, val ) =
            mkkeyval v
    in
        Json.Encode.object [ ( key, oeValue val ) ]


{-| Encode objects using the `TwoElementArray` scheme.
The first argument is a function that, for each possible value `a`, must return a `String` tag
describing it along with an `ObjectEncoding`.
-}
encodeSumTwoElementArray : (a -> ( String, ObjectEncoding )) -> a -> Value
encodeSumTwoElementArray mkkeyval v =
    let
        ( key, val ) =
            mkkeyval v
    in
        Json.Encode.list [ Json.Encode.string key, oeValue val ]


{-| Encode objects using the `TaggedObject` scheme.
The first argument is a function that, for each possible value `a`, must return a `String` tag
describing it along with an `ObjectEncoding`.
-}
encodeSumTaggedObject : String -> String -> (a -> ( String, ObjectEncoding )) -> a -> Value
encodeSumTaggedObject fieldname contentname mkkeyval v =
    let
        ( key, eval ) =
            mkkeyval v

        kp =
            ( fieldname, Json.Encode.string key )
    in
        case eval of
            EValue val ->
                Json.Encode.object [ kp, ( contentname, val ) ]

            EObject obj ->
                Json.Encode.object (kp :: obj)


{-| Helper for decoding enum-like sum types
-}
decodeSumUnaries : String -> Dict String a -> Json.Decode.Decoder a
decodeSumUnaries typename mapping =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case Dict.get s mapping of
                    Nothing ->
                        Json.Decode.fail ("Could not decode " ++ typename)

                    Just x ->
                        Json.Decode.succeed x
            )


{-| Helper function for decoding map-like objects. It takes a decoder for the key type and a decoder for the value type.
-}
decodeMap : Json.Decode.Decoder comparable -> Json.Decode.Decoder v -> Json.Decode.Decoder (Dict comparable v)
decodeMap decKey decVal =
    let
        decodeKeys =
            resmapM decodeKey

        decodeKey ( k, v ) =
            Result.map (\nk -> ( nk, v )) (Json.Decode.decodeString decKey k)
    in
        Json.Decode.map Dict.fromList (customDecoder (Json.Decode.keyValuePairs decVal) decodeKeys)


{-| Helper function for encoding map-like objects. It takes an encoder for the key type and an encoder for the value type
-}
encodeMap : (comparable -> Json.Encode.Value) -> (v -> Json.Encode.Value) -> Dict comparable v -> Json.Encode.Value
encodeMap encKey encVal =
    let
        encKey_ x =
            case Json.Decode.decodeValue Json.Decode.string (encKey x) of
                Err _ ->
                    toString x

                Ok s ->
                    s
    in
        Json.Encode.object << List.map (\( k, v ) -> ( encKey_ k, encVal v )) << Dict.toList


{-| An alias to `encodeMap` that is compatible with the naming convention from `elm-bridge`
-}
jsonEncDict : (comparable -> Json.Encode.Value) -> (v -> Json.Encode.Value) -> Dict comparable v -> Json.Encode.Value
jsonEncDict =
    encodeMap


{-| An alias to `decodeMap` that is compatible with the naming convention from `elm-bridge`
-}
jsonDecDict : Json.Decode.Decoder comparable -> Json.Decode.Decoder v -> Json.Decode.Decoder (Dict comparable v)
jsonDecDict =
    decodeMap


{-| A helper for set encoding
-}
encodeSet : (comparable -> Json.Encode.Value) -> Set comparable -> Json.Encode.Value
encodeSet e s =
    Json.Encode.list (List.map e (Set.toList s))


{-| A helper for set decoding
-}
decodeSet : Json.Decode.Decoder comparable -> Json.Decode.Decoder (Set comparable)
decodeSet d =
    Json.Decode.map Set.fromList (Json.Decode.list d)
