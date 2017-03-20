import System.Environment
import System.IO

import System.Directory
import System.FilePath

import Data.Either
import Data.List

import Control.Monad

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
instance ToJSON NodeOrAlbum
instance ToJSON AlbumTreeNode

main = do
  args <- getArgs
  case args of
       [] -> usage
       _:[] -> usage
       src:dest:[] -> writeNodeOrAlbum src dest
       _:_:_:_ -> usage

usage = putStrLn "usage: gen-album <src> <dest>"

writeNodeOrAlbum :: String -> String -> IO ()
writeNodeOrAlbum src dest = do
  eNodeOrAlbum <- genNodeOrAlbum src dest
  case eNodeOrAlbum of
    Left err ->
      putStrLn err
    Right nodeOrAlbum ->
      C.writeFile (dest </> "album.json") $ encode nodeOrAlbum

genNodeOrAlbum :: String -> String -> IO (Either String NodeOrAlbum)
genNodeOrAlbum src dest = do
  files <- filter (`notElem` [".",".."]) <$> getDirectoryContents src
  let afiles = map (\f -> src </> f) (sort files)
  imgs <- imgsOnly afiles
  subdirs <- dirsOnly afiles
  let icount = length imgs
      idirs = length subdirs
  if ((icount > 0) && (idirs > 0)) then do
    return $ Left $ "directory " ++ src ++ " contains both images and subdirs, this is not supported"
  else
    if length imgs > 0 then do
      a <- genAlbum src dest imgs
      return $ Right $ Leaf a
    else do
      en <- genNode src dest subdirs
      case en of
        Left err ->
          return $ Left $ err
        Right n ->
          return $ Right $ Subtree n

genNode :: String -> String -> [FilePath] -> IO (Either String AlbumTreeNode)
genNode src dest dirs = do
  ecFirst <- genNodeOrAlbum (head dirs) dest
  case ecFirst of
    Left err ->
      return $ Left $ err
    Right cFirst -> do
      ecRest <- mapM (\dir -> genNodeOrAlbum dir dest) (tail dirs)
      case lefts ecRest of
        [] -> do
          let cRest = rights ecRest
          return $ Right $ AlbumTreeNode { nodeTitle = src
                                         , childFirst = cFirst
                                         , childRest = cRest
                                         }
        errs -> do
          return $ Left $ head errs

genAlbum :: String -> String -> [(FilePath, DynamicImage)] -> IO Album
genAlbum src dest imgs = do
  pimgs <- sequence $ map (procImage dest) imgs
  return Album { title = last $ splitDirectories src
               , imageFirst = head pimgs
               , imageRest = tail pimgs
               }

procImage :: FilePath -> (FilePath, DynamicImage) -> IO Image
procImage d (f,i) = do
    let w = dynamicMap imageWidth i
        h = dynamicMap imageHeight i
        t = takeBaseName f
    srcSet <- procSrcSet d f i w h
    return Image { altText = t
                 , srcSetFirst = head srcSet
                 , srcSetRest = tail srcSet
                 }

procSrcSet :: FilePath -> FilePath -> DynamicImage -> Int -> Int -> IO [ImgSrc]
procSrcSet d f i w h = do
    rawImg <- raw d f w h
    putStrSameLn $ "processing " ++ (show f) ++ " "
    shrunken <- sequence $ map (shrinkImgSrc d f i w h) sizes
    return (rawImg : shrunken)

sizes :: [Int]
-- sizes = [1600, 800, 400, 200]
sizes = [400, 200, 100]

shrinkImgSrc :: FilePath -> FilePath -> DynamicImage -> Int -> Int -> Int -> IO ImgSrc
shrinkImgSrc d f i w h maxdim = do
    let fi = toFridayRGB $ convertRGB8 i
        (xsm, ysm) = shrink maxdim w h
        fism = resize Bilinear (ix2 ysm xsm) fi
        ism = toJuicyRGB fism
        fsm = (takeFileName (dropExtension f)) ++ "." ++ (show maxdim) ++ ".png"
        fsmpath = d </> fsm
    putStr $ show maxdim ++ "w "
    hFlush stdout
    savePngImage fsmpath $ ImageRGB8 ism
    return ImgSrc { url = fsm
                  , x = xsm
                  , y = ysm
                  }

raw :: FilePath -> FilePath -> Int -> Int -> IO ImgSrc
raw d fpath w h = do
    let f = takeFileName fpath
        dest = d </> f
    copyFile fpath dest
    putStrSameLn $ "copied " ++ f
    return ImgSrc { url = f
                  , x = w
                  , y = h
                  }

shrink :: Int -> Int -> Int -> (Int, Int)
shrink maxdim w h = let factor = fromIntegral maxdim / fromIntegral (max w h)
                        scale x = floor ((fromIntegral x) * factor)
                    in (scale w, scale h)


dirsOnly :: [FilePath] -> IO [FilePath]
dirsOnly = filterM doesDirectoryExist

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
    putStr "\r                                                                                                                                                          "
    putStr "\r"
    putStr s
    hFlush stdout
