{-# LANGUAGE TemplateHaskell #-}

import Elm.Derive
import Elm.Module

import Data.Proxy

import AlbumTypes

deriveBoth defaultOptions ''ImgSrc
deriveBoth defaultOptions ''Image
deriveBoth defaultOptions ''Album

main :: IO ()
main =
    putStrLn $ makeElmModule "Album"
    [ DefineElm (Proxy :: Proxy Album)
    , DefineElm (Proxy :: Proxy Image)
    , DefineElm (Proxy :: Proxy ImgSrc)
    ]
