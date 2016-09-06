-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.elm-bridge])"

{-# LANGUAGE TemplateHaskell #-}
import Elm.Derive
import Elm.Module

import Data.Proxy

data ImageSize
   = ImageSize
   { url :: String
   , x :: Int
   , y :: Int
   } deriving (Show, Eq)

type Picture = [ImageSize]

deriveBoth defaultOptions ''ImageSize

main :: IO ()
main =
    putStrLn $ makeElmModule "ImageSize"
    [ DefineElm (Proxy :: Proxy ImageSize)
    ]
