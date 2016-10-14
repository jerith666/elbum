import System.Environment

import System.Directory
import System.FilePath

import Codec.Picture hiding (Image)
import Codec.Picture.Types hiding (Image)

import Vision.Primitive
import Vision.Primitive.Shape
import Vision.Image hiding (Image, map)
import Vision.Image.Transform
import Vision.Image.JuicyPixels

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
procSrcSet f i w h = do let fi = toFridayRGB $ convertRGB8 i
                            (xsm, ysm) = shrink 200 w h
                            fism = resize NearestNeighbor (ix2 ysm xsm) fi
                            ism = toJuicyRGB fism
                            fsmpath = "/tmp/" ++ (takeFileName (dropExtension f)) ++ ".sm.png"
                        savePngImage fsmpath $ ImageRGB8 ism
                        return [ ImgSrc { url = f
                                        , x = w
                                        , y = h
                                        }
                               , ImgSrc { url = fsmpath
                                        , x = xsm
                                        , y = ysm
                                        }
                               ]

shrink :: Int -> Int -> Int -> (Int, Int)
shrink maxdim w h = let factor = fromIntegral maxdim / fromIntegral (max w h)
                        scale x = floor ((fromIntegral x) * factor)
                    in (scale w, scale h)



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
