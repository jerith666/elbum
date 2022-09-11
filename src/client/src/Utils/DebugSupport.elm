module Utils.DebugSupport exposing (debugString, log)


log : String -> a -> a
log str val =
    val


debugString : a -> String
debugString val =
    "Debug.toString not supported in --optimize mode"
