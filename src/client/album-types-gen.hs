import Elm.Module
import Elm.Versions

import Data.Proxy

import AlbumTypes

main :: IO ()
main =
    putStrLn $ makeElmModuleWithVersion Elm0p19 "Album"
    [ DefineElm (Proxy :: Proxy AlbumList)
    , DefineElm (Proxy :: Proxy AlbumOrList)
    , DefineElm (Proxy :: Proxy Album)
    , DefineElm (Proxy :: Proxy Image)
    , DefineElm (Proxy :: Proxy ImgSrc)
    ]
