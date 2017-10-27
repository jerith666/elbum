import System.Environment
import System.IO

import System.Directory
import System.FilePath

import Data.Either
import Data.List
import Data.Maybe

import Control.Monad

import Text.Regex

import Codec.Picture hiding (Image)
import Codec.Picture.Types hiding (Image)

import Vision.Primitive
import Vision.Primitive.Shape
import Vision.Image hiding (Image, map)
import Vision.Image.Transform
import Vision.Image.JuicyPixels

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as C

import AlbumTypes

main = do
  args <- getArgs
  case args of
       [] -> usage
       _:[] -> usage
       src:dest:[] -> writeNodeOrAlbum src dest
       _:_:_:_ -> usage

usage = putStrLn "usage: gen-album <src> <dest>"

thumbFilename = "thumbnail"

writeNodeOrAlbum :: String -> String -> IO ()
writeNodeOrAlbum src dest = do
  eNodeOrAlbum <- genNodeOrAlbum src src dest True
  case eNodeOrAlbum of
    Left err ->
      putStrLn err
    Right nodeOrAlbum ->
      C.writeFile (dest </> "album.json") $ encode nodeOrAlbum

genNodeOrAlbum :: FilePath -> FilePath -> FilePath -> Bool -> IO (Either String NodeOrAlbum)
genNodeOrAlbum srcRoot src dest autoThumb = do
  files <- filter (`notElem` [".","..",thumbFilename]) <$> getDirectoryContents src
  let afiles = map (\f -> src </> f) (sort files)
  imgs <- imgsOnly afiles
  subdirs <- dirsOnly afiles
  let icount = length imgs
      idirs = length subdirs
  if ((icount > 0) && (idirs > 0)) then do
    return $ Left $ "directory " ++ src ++ " contains both images and subdirs, this is not supported"
  else
    if length imgs > 0 then do
      aOrErr <- genAlbum srcRoot src dest imgs
      case aOrErr of
        Left err ->
          return $ Left $ err
        Right a ->
          return $ Right $ Leaf a
    else do
      en <- genNode srcRoot src dest autoThumb subdirs
      case en of
        Left err ->
          return $ Left $ err
        Right n ->
          return $ Right $ Subtree n

genNode :: FilePath -> FilePath -> FilePath -> Bool -> [FilePath] -> IO (Either String AlbumTreeNode)
genNode srcRoot src dest autoThumb dirs = do
  ecFirst <- genNodeOrAlbum srcRoot (head dirs) dest False
  case ecFirst of
    Left err ->
      return $ Left $ err
    Right cFirst -> do
      ecRest <- mapM (\dir -> genNodeOrAlbum srcRoot dir dest False) (tail dirs)
      case lefts ecRest of
        [] -> do
          let cRest = rights ecRest
              childImages = getChildImages $ cFirst : cRest
          thumbOrErr <- findThumb srcRoot src dest childImages
          case thumbOrErr of
            Left err ->
              if autoThumb then
                return $ Right $ AlbumTreeNode { nodeTitle = titleForDir src
                                               , nodeThumbnail = head childImages
                                               , childFirst = cFirst
                                               , childRest = cRest
                                               }
              else
                return $ Left $ err
            Right thumb ->
              return $ Right $ AlbumTreeNode { nodeTitle = titleForDir src
                                             , nodeThumbnail = thumb
                                             , childFirst = cFirst
                                             , childRest = cRest
                                             }
        errs -> do
          return $ Left $ head errs

getChildImages :: [NodeOrAlbum] -> [Image]
getChildImages nodeOrAlbums =
  concat $ map getChildImages1 nodeOrAlbums

getChildImages1 :: NodeOrAlbum -> [Image]
getChildImages1 nodeOrAlbum =
  case nodeOrAlbum of
    Subtree albumTreeNode ->
      getChildImages $ (childFirst albumTreeNode) : (childRest albumTreeNode)
    Leaf album ->
      (imageFirst album) : (imageRest album)

genAlbum :: FilePath -> FilePath -> FilePath -> [(FilePath, DynamicImage)] -> IO (Either String Album)
genAlbum srcRoot src dest imgs = do
  pimgs <- sequence $ map (procImage srcRoot dest) imgs
  thumbOrErr <- findThumb srcRoot src dest pimgs
  case thumbOrErr of
    Left err ->
      return $ Left $ err
    Right thumb ->
      return $ Right $ Album { title = titleForDir src
                             , thumbnail = thumb
                             , imageFirst = head pimgs
                             , imageRest = tail pimgs
                             }

titleForDir :: String -> String
titleForDir dir =
  let dirName = last $ splitDirectories dir
  in subRegex (mkRegex "^[0-9]+_") dirName ""

findThumb :: FilePath -> FilePath -> FilePath -> [Image] -> IO (Either String Image)
findThumb srcRoot src dest images = do
  let thumbLink = src </> thumbFilename
  thumbLinkFileExists <- doesFileExist thumbLink
  if thumbLinkFileExists then do
    thumbLinkExists <- pathIsSymbolicLink thumbLink
    if thumbLinkExists then do
      thumbPath <- readLink thumbLink
      if isAbsolute thumbPath then do
        return $ Left $ src ++ " thumbnail link must point to a relative path, but is absolute: " ++ thumbPath
      else do
        let thumbDest = makeRelative dest $ fst $ destForRaw src dest thumbPath
            isThumb i = equalFilePath thumbDest $ url $ srcSetFirst i
            thumb = listToMaybe $ filter isThumb images
        case thumb of
          Nothing -> do
            return $ Left $ src ++ " thumbnail '" ++ thumbPath ++ "' (" ++ thumbDest ++ ") does not point to any images in this album: " ++ (concat $ map (\i -> url $ srcSetFirst i) images)
          Just t -> do
            return $ Right $ t
    else do
      return $ Left $ thumbLink ++ " is not a symbolic link"
  else do
    return $ Left $ src ++ " does not contain a 'thumbnail' file"

readLink :: FilePath -> IO FilePath
readLink f = do
  isLink <- pathIsSymbolicLink f
  if isLink then do
    putStrLn $ "file " ++ f ++ " is link"
    target <- getSymbolicLinkTarget f
    putStrLn $ "file " ++ f ++ " resolves to " ++ target
    if isAbsolute target then do
      putStrLn $ target ++ " is absolute"
      readLink target
    else do
      let ftgt = takeDirectory f </> target
      putStrLn $ target ++ " made relative: " ++ ftgt
      readLink ftgt
  else do
    putStrLn $ "file " ++ f ++ " is not link"
    return f

procImage :: FilePath -> FilePath -> (FilePath, DynamicImage) -> IO Image
procImage s d (f,i) = do
    let w = dynamicMap imageWidth i
        h = dynamicMap imageHeight i
        t = takeBaseName f
    srcSet <- procSrcSet s d f i w h
    return Image { altText = t
                 , srcSetFirst = head srcSet
                 , srcSetRest = tail srcSet
                 }

procSrcSet :: FilePath -> FilePath -> FilePath -> DynamicImage -> Int -> Int -> IO [ImgSrc]
procSrcSet s d f i w h = do
    rawImg <- raw s d f w h
    putStrSameLn $ "processing " ++ (show f) ++ " "
    shrunken <- sequence $ map (shrinkImgSrc s d f i w h) sizes
    return (rawImg : shrunken)

sizes :: [Int]
-- sizes = [1600, 800, 400, 200]
sizes = [400, 200, 100]

shrinkImgSrc :: FilePath -> FilePath -> FilePath -> DynamicImage -> Int -> Int -> Int -> IO ImgSrc
shrinkImgSrc s d f i w h maxdim = do
    let fi = toFridayRGB $ convertRGB8 i
        (xsm, ysm) = shrink maxdim w h
        fism = resize Bilinear (ix2 ysm xsm) fi
        ism = toJuicyRGB fism
        fsmpath = fst $ destForShrink maxdim s d f
    putStr $ show maxdim ++ "w "
    hFlush stdout
    createDirectoryIfMissing True $ takeDirectory fsmpath
    savePngImage fsmpath $ ImageRGB8 ism
    return ImgSrc { url = makeRelative d fsmpath
                  , x = xsm
                  , y = ysm
                  }

raw :: FilePath -> FilePath -> FilePath -> Int -> Int -> IO ImgSrc
raw s d fpath w h = do
    let (f,dest) = destForRaw s d fpath
    createDirectoryIfMissing True $ takeDirectory dest
    copyFile fpath dest
    putStrSameLn $ "copied " ++ f
    return ImgSrc { url = makeRelative d dest
                  , x = w
                  , y = h
                  }

destForRaw :: FilePath -> FilePath -> FilePath -> (FilePath, FilePath)
destForRaw =
  destFor takeFileName

destForShrink :: Int -> FilePath -> FilePath -> FilePath -> (FilePath, FilePath)
destForShrink maxdim =
  destFor (\f -> (takeFileName (dropExtension f)) ++ "." ++ (show maxdim) ++ ".png")

destFor :: (FilePath -> FilePath) -> FilePath -> FilePath -> FilePath -> (FilePath, FilePath)
destFor fNameMaker src dest fileInSrc =
  let srel = takeDirectory $ makeRelative src fileInSrc
      fName = fNameMaker fileInSrc
  in (dest </> srel </> fName, fName)

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
                     return $ (maybeToList fo) ++ fos

imgOnly :: FilePath -> IO (Maybe (FilePath, DynamicImage))
imgOnly f = do
    loadResult <- readImage f
    case loadResult of
         Left err -> do return Nothing
         Right img ->
             do
                 putStrSameLn $ "loaded " ++ (show f)
                 return $ Just (f, img)

putStrSameLn :: String -> IO ()
putStrSameLn s = do
    putStr "\r                                                                                                                                                          "
    putStr "\r"
    putStr s
    hFlush stdout
