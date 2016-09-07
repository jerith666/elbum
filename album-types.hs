-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.elm-bridge])"

{-# LANGUAGE TemplateHaskell #-}
import Elm.Derive
import Elm.Module

import Data.Proxy

data ImgSrc
   = ImgSrc
   { url :: String
   , x :: Int
   , y :: Int
   } deriving (Show, Eq)

data ImgSrcSet
   = ImgSrcSet
   { srcs :: [ImgSrc]
   } deriving (Show, Eq)

data Image
   = Image
   { altText :: String
   , srcSet :: ImgSrcSet
   } deriving (Show, Eq)

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
