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

{-| Each level of the album tree has a title, a thumbnail,
    and at least one child.
    Each child is either a subtree or a "leaf" album. -}
data AlbumTreeNode
   = AlbumTreeNode
   { nodeTitle :: String
   , childFirst :: NodeOrAlbum
   , childRest :: [NodeOrAlbum]
   , nodeThumbnail :: Image
   } deriving (Generic, Show, Eq)

{-| A union type of either a tree node or a "leaf" album. -}
data NodeOrAlbum
   = Subtree AlbumTreeNode
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
   } deriving (Generic, Show, Eq)

deriveBoth defaultOptions ''AlbumTreeNode
deriveBoth defaultOptions ''NodeOrAlbum
deriveBoth defaultOptions ''Album
deriveBoth defaultOptions ''Image
deriveBoth defaultOptions ''ImgSrc
