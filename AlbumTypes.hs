-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.elm-bridge])"
{-# LANGUAGE DeriveGeneric #-}

module AlbumTypes
( ImgSrc(..)
, ImgSrcSet(..)
, Image(..)
, Album(..)
) where

import Data.Aeson
import GHC.Generics

data ImgSrc
   = ImgSrc
   { url :: String
   , x :: Int
   , y :: Int
   } deriving (Generic, Show, Eq)

data ImgSrcSet
   = ImgSrcSet
   { srcs :: [ImgSrc]
   } deriving (Generic, Show, Eq)

data Image
   = Image
   { altText :: String
   , srcSet :: ImgSrcSet
   } deriving (Generic, Show, Eq)

data Album
   = Album
   { title :: String
   , images :: [Image]
   } deriving (Generic, Show, Eq)
