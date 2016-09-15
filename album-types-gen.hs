{-# LANGUAGE TemplateHaskell #-}

import Elm.Derive
import Elm.Module

import Data.Proxy

import AlbumTypes

deriveBoth defaultOptions ''ImgSrc
deriveBoth defaultOptions ''ImgSrcSet
deriveBoth defaultOptions ''Image

main :: IO ()
main =
    putStrLn $ makeElmModule "Image"
    [ DefineElm (Proxy :: Proxy Image)
    , DefineElm (Proxy :: Proxy ImgSrcSet)
    , DefineElm (Proxy :: Proxy ImgSrc)
    ]
