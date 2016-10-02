--nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.ihaskell-juicypixels])"

import System.Environment

import System.Directory
import System.FilePath

import Codec.Picture hiding (Image)
import Codec.Picture.Types hiding (Image)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C

import AlbumTypes

instance ToJSON ImgSrc

instance ToJSON Image

instance ToJSON Album

main = do
  args <- getArgs
  case args of
       [] -> usage
       _:[] -> usage
       src:dest:[] -> genAlbum src dest
       _:_:_:_ -> usage

usage = putStrLn "usage: gen-album <src> <dest>"

genAlbum src dest = do
  files <- getDirectoryContents src
  let afiles = map (\f -> makeRelative dest $ src </> f) files
  imgs <- imgsOnly afiles
  pimgs <- sequence $ map procImage imgs
  let a = Album { title = last $ splitDirectories src
                , images = pimgs
                }
  C.putStrLn $ encode a

procImage :: (FilePath, DynamicImage) -> IO Image
procImage (f,i) = do let w = dynamicMap imageWidth i
                         h = dynamicMap imageHeight i
                         t = takeBaseName f
                     srcSet <- procSrcSet f i w h
                     return Image { altText = t
                                  , srcSet = srcSet
                                  }

procSrcSet :: FilePath -> DynamicImage -> Int -> Int -> IO [ImgSrc]
procSrcSet f i w h = return [ ImgSrc { url = f
                                     , x = w
                                     , y = h
                                     }
                            ]

imgsOnly :: [FilePath] -> IO [(FilePath, DynamicImage)]
imgsOnly [] = return []
imgsOnly (f:fs) = do fo <- imgOnly f
                     fos <- imgsOnly fs
                     return $ fo ++ fos

imgOnly :: FilePath -> IO [(FilePath, DynamicImage)]
imgOnly f = do loadResult <- readImage f
               case loadResult of
                    Left err -> do return []
                    Right img -> do return [(f, img)]
