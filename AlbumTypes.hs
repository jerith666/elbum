-- nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.elm-bridge])"

module AlbumTypes
( ImgSrc
, ImgSrcSet
, Image
) where

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

