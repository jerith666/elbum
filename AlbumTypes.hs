{-# LANGUAGE DeriveGeneric #-}

module AlbumTypes
( ImgSrc(..)
, Image(..)
, Album(..)
, NodeOrAlbum(..)
, AlbumTreeNode(..)
) where

import Data.Aeson
import GHC.Generics

{-| Each level of the album tree has a title, and a list children.
    each child is either a subtree or an "leaf" album. -}
data AlbumTreeNode
   = AlbumTreeNode
   { nodeTitle :: String
   , childFirst :: NodeOrAlbum
   , childRest :: [NodeOrAlbum]
   }

{-| A union type of either a tree node or a "leaf" album. -}
data NodeOrAlbum
   = Subtree AlbumTreeNode
   | Leaf Album

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
