{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Wcompat                    #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wredundant-constraints     #-}
{-# OPTIONS_GHC -Werror                     #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module AlbumTypes
( ImgSrc(..)
, Image(..)
, Album(..)
, AlbumOrList(..)
, AlbumList(..)
) where

import Control.Parallel.Strategies

import GHC.Generics

import Elm.Derive

{-| a list of albums has a title, a thumbnail, and at least one child.
    Each child is either a subtree or a "leaf" album. -}
data AlbumList
   = AlbumList
   { listTitle :: String
   , childFirst :: AlbumOrList
   , childRest :: [AlbumOrList]
   , listThumbnail :: Image
   } deriving (Generic, Show, Eq)

{-| A union type of either a list of albums or a "leaf" album. -}
data AlbumOrList
   = List AlbumList
   | Leaf Album
   deriving (Generic, Show, Eq)

{-| A single photo album has a title, a thumbnail,
    and a collection of at least one image. -}
data Album
   = Album
   { title :: String
   , thumbnail :: Image
   , imageFirst :: Image
   , imageRest :: [Image]
   } deriving (Generic, Show, Eq)

{-| Each image in the album has alt-text and a srcset with at least
    one element.  The srcset aligns with the html <img/> tag's srcset
    attribute, which is basically just a list of the same image at
    different resolutions. -}
data Image
   = Image
   { altText :: String
   , srcSetFirst :: ImgSrc
   , srcSetRest :: [ImgSrc]
   } deriving (Generic, Show, Eq)

{-| A single image source, which comes from a particular URL and has a
    known fixed size. -}
data ImgSrc
   = ImgSrc
   { url :: String
   , x :: Int
   , y :: Int
   } deriving (Generic, Show, Eq, NFData)

deriveBoth defaultOptions ''AlbumList
deriveBoth defaultOptions ''AlbumOrList
deriveBoth defaultOptions ''Album
deriveBoth defaultOptions ''Image
deriveBoth defaultOptions ''ImgSrc
