module Utils.DebugSupport exposing (debugString, log)


log : String -> a -> a
log str val =
    --val
    Debug.log str val


debugString : a -> String
debugString val =
    --"Debug.toString not supported in --optimize mode"
    Debug.toString val
