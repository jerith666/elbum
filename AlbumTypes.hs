-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.elm-bridge])"
{-# LANGUAGE DeriveGeneric #-}

module AlbumTypes
( ImgSrc(..)
, ImgSrcSet(..)
, Image(..)
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

instance ToJSON ImgSrc

instance ToJSON ImgSrcSet

instance ToJSON Image
