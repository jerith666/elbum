{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AlbumTypes
( ImgSrc(..)
, Image(..)
, Album(..)
, NodeOrAlbum(..)
, AlbumTreeNode(..)
) where

import GHC.Generics

import Elm.Derive

{-| Each level of the album tree has a title, and a list of children.
    Each child is either a subtree or a "leaf" album. -}
data AlbumTreeNode
   = AlbumTreeNode
   { nodeTitle :: String
   , childFirst :: NodeOrAlbum
   , childRest :: [NodeOrAlbum]
   } deriving (Generic, Show, Eq)

{-| A union type of either a tree node or a "leaf" album. -}
data NodeOrAlbum
   = Subtree AlbumTreeNode
   | Leaf Album
   deriving (Generic, Show, Eq)

{-| A single photo album has a title and a collection of at least one image.
    Future: add sub-albums. -}
data Album
   = Album
   { title :: String
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
   } deriving (Generic, Show, Eq)

deriveBoth defaultOptions{sumEncoding=TwoElemArray} ''AlbumTreeNode
deriveBoth defaultOptions{sumEncoding=TwoElemArray} ''NodeOrAlbum
deriveBoth defaultOptions{sumEncoding=TwoElemArray} ''Album
deriveBoth defaultOptions{sumEncoding=TwoElemArray} ''Image
deriveBoth defaultOptions{sumEncoding=TwoElemArray} ''ImgSrc
