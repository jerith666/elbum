{-# LANGUAGE TemplateHaskell #-}

import Elm.Derive
import Elm.Module
import Elm.Versions

import Data.Proxy

import AlbumTypes

deriveBoth defaultOptions ''ImgSrc
deriveBoth defaultOptions ''Image
deriveBoth defaultOptions ''Album
deriveBoth defaultOptions ''NodeOrAlbum
deriveBoth defaultOptions ''AlbumTreeNode

main :: IO ()
main =
    putStrLn $ makeElmModuleWithVersion Elm0p18 "Album"
    [ DefineElm (Proxy :: Proxy AlbumTreeNode)
    , DefineElm (Proxy :: Proxy NodeOrAlbum)
    , DefineElm (Proxy :: Proxy Album)
    , DefineElm (Proxy :: Proxy Image)
    , DefineElm (Proxy :: Proxy ImgSrc)
    ]
