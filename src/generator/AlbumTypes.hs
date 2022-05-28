{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module AlbumTypes
  ( ImgSrc (..),
    Image (..),
    Album (..),
    AlbumOrList (..),
    AlbumList (..),
  )
where

import Control.Parallel.Strategies
import Elm.Derive
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

-- | a list of albums has a title, a thumbnail, and at least one child.
--    Each child is either a subtree or a "leaf" album.
data AlbumList = AlbumList
  { listTitle :: String,
    childFirst :: AlbumOrList,
    childRest :: [AlbumOrList],
    listThumbnail :: Image
  }
  deriving (Generic, Show, Eq)

-- derived automatically
instance ToJSON AlbumList where
instance FromJSON AlbumList where

-- | A union type of either a list of albums or a "leaf" album.
data AlbumOrList
  = List AlbumList
  | Leaf Album
  deriving (Generic, Show, Eq)

-- derived automatically
instance ToJSON AlbumOrList where
instance FromJSON AlbumOrList where

-- | A single photo album has a title, a thumbnail,
--    and a collection of at least one image.
data Album = Album
  { title :: String,
    thumbnail :: Image,
    imageFirst :: Image,
    imageRest :: [Image]
  }
  deriving (Generic, Show, Eq)

-- derived automatically
instance ToJSON Album where
instance FromJSON Album where

-- | Each image in the album has alt-text and a srcset with at least
--    one element.  The srcset aligns with the html <img/> tag's srcset
--    attribute, which is basically just a list of the same image at
--    different resolutions.
data Image = Image
  { altText :: String,
    srcSetFirst :: ImgSrc,
    srcSetRest :: [ImgSrc]
  }
  deriving (Generic, Show, Eq)

-- derived automatically
instance ToJSON Image where
instance FromJSON Image where

-- | A single image source, which comes from a particular URL and has a
--    known fixed size.
data ImgSrc = ImgSrc
  { url :: String,
    x :: Int,
    y :: Int
  }
  deriving (Generic, Show, Eq, NFData)

-- derived automatically
instance ToJSON ImgSrc where
instance FromJSON ImgSrc where

deriveElmDef defaultOptions ''AlbumList
deriveElmDef defaultOptions ''AlbumOrList
deriveElmDef defaultOptions ''Album
deriveElmDef defaultOptions ''Image
deriveElmDef defaultOptions ''ImgSrc
