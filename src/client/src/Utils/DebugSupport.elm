module Utils.DebugSupport exposing (debugString, log)


log : String -> a -> a
log str val =
    Debug.log str val


debugString : a -> String
debugString val =
    Debug.toString val
