import System.Environment
import System.IO

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
  let afiles = map (\f -> src </> f) files
  imgs <- imgsOnly afiles
  pimgs <- sequence $ map (procImage dest) imgs
  let a = Album { title = last $ splitDirectories src
                , images = pimgs
                }
  C.putStrLn $ encode a

procImage :: FilePath -> (FilePath, DynamicImage) -> IO Image
procImage d (f,i) = do
    let w = dynamicMap imageWidth i
        h = dynamicMap imageHeight i
        t = takeBaseName f
    srcSet <- procSrcSet d f i w h
    return Image { altText = t
                 , srcSet = srcSet
                 }

procSrcSet :: FilePath -> FilePath -> DynamicImage -> Int -> Int -> IO [ImgSrc]
procSrcSet d f i w h = do
    let rawImg = raw f w h
    putStrSameLn $ "processing " ++ (show f) ++ " "
    shrunken <- sequence $ map (shrinkImgSrc d f i w h) sizes
    return (shrunken ++ [rawImg])

sizes :: [Int]
sizes = [200, 400] -- , 800, 1600]

shrinkImgSrc :: FilePath -> FilePath -> DynamicImage -> Int -> Int -> Int -> IO ImgSrc
shrinkImgSrc d f i w h maxdim = do
    let fi = toFridayRGB $ convertRGB8 i
        (xsm, ysm) = shrink maxdim w h
        fism = resize Bilinear (ix2 ysm xsm) fi
        ism = toJuicyRGB fism
        fsmpath = d </> (takeFileName (dropExtension f)) ++ "." ++ (show maxdim) ++ ".png"
    putStr $ show maxdim ++ "w "
    hFlush stdout
    savePngImage fsmpath $ ImageRGB8 ism
    return ImgSrc { url = fsmpath
                  , x = w
                  , y = h
                  }

raw :: FilePath -> Int -> Int -> ImgSrc
raw f w h = ImgSrc { url = f
                   , x = w
                   , y = h
                   }

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
imgOnly f = do
    loadResult <- readImage f
    case loadResult of
         Left err -> do return []
         Right img ->
             do
                 putStrSameLn $ "loaded " ++ (show f)
                 return [(f, img)]

putStrSameLn :: String -> IO ()
putStrSameLn s = do
    putStr "\r                                                              "
    putStr "\r"
    putStr s
    hFlush stdout
