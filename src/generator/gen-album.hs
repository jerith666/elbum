import System.Environment
import System.IO
import System.Exit

import System.Directory
import System.FilePath

import Data.Either
import Data.List
import Data.Maybe

import Control.Monad

import Text.Regex

import Codec.Picture hiding (Image)
import Codec.Picture.Types hiding (Image)
import Codec.Picture.Metadata hiding (Image)

import Vision.Primitive
import Vision.Primitive.Shape
import Vision.Image hiding (Image, map)
import Vision.Image.Transform
import Vision.Image.JuicyPixels

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as C

import AlbumTypes

--
-- main & usage
--

main = do
  args <- getArgs
  case args of
       src:dest:[] -> writeAlbumOrList src dest
       _ -> usage

usage = do
  putStrLn "usage: gen-album <src> <dest>"
  exitWith $ ExitFailure 1

--
-- configuration
--

thumbFilename = "thumbnail"

sizes :: [Int]
sizes = [1600, 1400, 1200, 1000, 800, 400, 200]

--
-- Album & AlbumList functions
--

writeAlbumOrList :: String -> String -> IO ()
writeAlbumOrList src dest = do
  eAlbumOrList <- genAlbumOrList src src dest True
  case eAlbumOrList of
    Left err -> do
      putStrLn ""
      putStrLn err
      exitWith $ ExitFailure 1
    Right albumOrList ->
      C.writeFile (dest </> "album.json") $ encode albumOrList

genAlbumOrList :: FilePath -> FilePath -> FilePath -> Bool -> IO (Either String AlbumOrList)
genAlbumOrList srcRoot src dest autoThumb = do
  files <- filter (`notElem` [".","..",thumbFilename]) <$> getDirectoryContents src
  let afiles = map (\f -> src </> f) (sort files)
  epimgs <- procImgsOnly srcRoot dest afiles
  case epimgs of
    Left e -> do
      return $ Left e
    Right pimgs -> do
      subdirs <- dirsOnly afiles
      let icount = length pimgs
          idirs = length subdirs
      if ((icount > 0) && (idirs > 0)) then do
        return $ Left $ "directory " ++ src ++ " contains both images and subdirs, this is not supported"
      else
        if length pimgs > 0 then do
          aOrErr <- genAlbum srcRoot src dest pimgs
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
              return $ Right $ List n

genNode :: FilePath -> FilePath -> FilePath -> Bool -> [FilePath] -> IO (Either String AlbumList)
genNode srcRoot src dest autoThumb dirs = do
  ecFirst <- genAlbumOrList srcRoot (head dirs) dest False
  case ecFirst of
    Left err ->
      return $ Left $ err
    Right cFirst -> do
      ecRest <- mapM (\dir -> genAlbumOrList srcRoot dir dest False) (tail dirs)
      case lefts ecRest of
        [] -> do
          let cRest = rights ecRest
              childImages = getChildImages $ cFirst : cRest
          thumbOrErr <- findThumb srcRoot src dest childImages
          case thumbOrErr of
            Left err ->
              if autoThumb then
                return $ Right $ AlbumList { listTitle = titleForDir src
                                           , listThumbnail = head childImages
                                           , childFirst = cFirst
                                           , childRest = cRest
                                           }
              else
                return $ Left $ err
            Right thumb ->
              return $ Right $ AlbumList { listTitle = titleForDir src
                                         , listThumbnail = thumb
                                         , childFirst = cFirst
                                         , childRest = cRest
                                         }
        errs -> do
          return $ Left $ concat $ intersperse "\n" errs

getChildImages :: [AlbumOrList] -> [Image]
getChildImages albumOrLists =
  concat $ map getChildImages1 albumOrLists

getChildImages1 :: AlbumOrList -> [Image]
getChildImages1 albumOrList =
  case albumOrList of
    List albumTreeNode ->
      getChildImages $ (childFirst albumTreeNode) : (childRest albumTreeNode)
    Leaf album ->
      (imageFirst album) : (imageRest album)

genAlbum :: FilePath -> FilePath -> FilePath -> [Image] -> IO (Either String Album)
genAlbum srcRoot src dest imgs = do
  thumbOrErr <- findThumb srcRoot src dest imgs
  case thumbOrErr of
    Left err ->
      return $ Left $ err
    Right thumb ->
      return $ Right $ Album { title = titleForDir src
                             , thumbnail = thumb
                             , imageFirst = head imgs
                             , imageRest = tail imgs
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
      if isAbsolute $ makeRelative srcRoot thumbPath then do
        return $ Left $ src ++ " thumbnail link must point to a path inside " ++ srcRoot ++", but does not: " ++ thumbPath
      else do
        let thumbDest = fst $ destForRaw srcRoot dest thumbPath
            isThumb i = equalFilePath thumbDest $ (dest </> (url $ srcSetFirst i))
            thumb = listToMaybe $ filter isThumb images
        case thumb of
          Nothing -> do
            return $ Left $ src ++ " thumbnail '" ++ thumbPath ++ "' (" ++ thumbDest ++ ") does not point to any images in this album: " ++ (concat $ map (\i -> dest </> (url $ srcSetFirst i)) images)
          Just t -> do
            return $ Right $ t
    else do
      return $ Left $ thumbLink ++ " is not a symbolic link"
  else do
    return $ Left $ src ++ " does not contain a 'thumbnail' file"

--
-- Single-image functions
--

procImgsOnly :: FilePath -> FilePath -> [FilePath] -> IO (Either String [Image])
procImgsOnly _ _ [] = return $ Right []
procImgsOnly s d (f:fs) = do
  mi <- procOrReuse s d f
  case mi of
    Nothing -> do
      is <- procImgsOnly s d fs
      return is
    Just pdiOrI -> do
      -- this ordering is key to ensuring memory usage remains relatively constant
      -- we have to process the first image completely (load it, save it out at all
      -- sizes, and convert to an Image) before moving on to the others
      epi <- either (procImage s d) (return . Right) pdiOrI
      case epi of
        Left e -> do
          return $ Left e
        Right pi -> do
          eis <- procImgsOnly s d fs
          case eis of
            Left e ->
              return $ Left e
            Right is ->
              return $ Right $ pi:is

procOrReuse :: FilePath -> FilePath -> FilePath -> IO (Maybe (Either (FilePath, (DynamicImage, Metadatas)) Image))
procOrReuse s d f = do
  mi <- imgOnly f
  case mi of
    Nothing -> do
      return Nothing
    Just i -> do
      let rawDest = fst $ destForRaw s d f
          shrinkDests = map (\maxwidth -> ( maxwidth
                                          , fst $ destForShrink maxwidth s d f))
                            sizes
      destsExist <- sequence $ map doesFileExist $ rawDest : map snd shrinkDests
      if and destsExist then do
        let mw = Codec.Picture.Metadata.lookup Width $ snd $ snd i
            mh = Codec.Picture.Metadata.lookup Height $ snd $ snd i
            wh = maybeTuple (mw, mh)
        case wh of
          Just (ww, hw) -> do
            let t = takeBaseName f
                w = fromIntegral ww
                h = fromIntegral hw
                srcSetFirst = ImgSrc { url = makeRelative d rawDest
                                     , x = w
                                     , y = h
                                     }
                sdToImgSrc sd =
                  let (xsm, ysm) = shrink (fst sd) w h
                  in
                  ImgSrc { url = makeRelative d $ snd sd
                         , x = xsm
                         , y = ysm
                         }
                srcSetRest = map sdToImgSrc shrinkDests
            return $ Just $ Right $ Image { altText = t
                                          , srcSetFirst = srcSetFirst
                                          , srcSetRest = srcSetRest
                                          }
          Nothing -> do
            return Nothing
      else do
        return $ Just $ Left $ i

imgOnly :: FilePath -> IO (Maybe (FilePath, (DynamicImage, Metadatas)))
imgOnly f = do
    loadResult <- readImageWithMetadata f
    case loadResult of
         Left err -> do return Nothing
         Right img ->
             do
                 putStrSameLn $ "loaded " ++ (show f)
                 return $ Just (f, img)

procImage :: FilePath -> FilePath -> (FilePath, (DynamicImage, Metadatas)) -> IO (Either String Image)
procImage s d (f,i) = do
    let w = dynamicMap imageWidth $ fst i
        h = dynamicMap imageHeight $ fst i
        mw = Codec.Picture.Metadata.lookup Width $ snd i
        mh = Codec.Picture.Metadata.lookup Height $ snd i
        mwh = maybeTuple (mw, mh)
        t = takeBaseName f
    case mwh of
      Nothing ->
        return $ Left $ "image " ++ f ++ " has no size metadata, album regen will silently drop it"
      Just (ww, hh) ->
        if fromIntegral ww == w && fromIntegral hh == h then do
          srcSet <- procSrcSet s d f (fst i) w h
          return $ Right $ Image { altText = t
                                 , srcSetFirst = head srcSet
                                 , srcSetRest = tail srcSet
                                 }
        else do
          return $ Left $ "image " ++ f ++ " has intrinsic w*h of " ++ (show w) ++ "*" ++ (show h) ++ " but metadata w*h of " ++ (show ww) ++ "*" ++ (show hh) ++ "; to repair, load in The Gimp, then choose file -> overwrite"

procSrcSet :: FilePath -> FilePath -> FilePath -> DynamicImage -> Int -> Int -> IO [ImgSrc]
procSrcSet s d f i w h = do
    rawImg <- raw s d f w h
    putStrSameLn $ "processing " ++ (show f) ++ " "
    shrunken <- sequence $ map (shrinkImgSrc s d f i w h) sizes
    return (rawImg : shrunken)

shrinkImgSrc :: FilePath -> FilePath -> FilePath -> DynamicImage -> Int -> Int -> Int -> IO ImgSrc
shrinkImgSrc s d f i w h maxwidth = do
    let (xsm, ysm) = shrink maxwidth w h
        fsmpath = fst $ destForShrink maxwidth s d f
    createDirectoryIfMissing True $ takeDirectory fsmpath
    fsmpathExists <- doesFileExist fsmpath
    if fsmpathExists then do
      putStr $ show maxwidth ++ "(e) "
      hFlush stdout
      return ImgSrc { url = makeRelative d fsmpath
                    , x = xsm
                    , y = ysm
                    }
    else do 
      let fi = toFridayRGB $ convertRGB8 i
          fism = resize Bilinear (ix2 ysm xsm) fi
          ism = toJuicyRGB fism
      putStr $ show maxwidth ++ "w "
      hFlush stdout
      savePngImage fsmpath $ ImageRGB8 ism
      return ImgSrc { url = makeRelative d fsmpath
                    , x = xsm
                    , y = ysm
                    }

raw :: FilePath -> FilePath -> FilePath -> Int -> Int -> IO ImgSrc
raw s d fpath w h = do
    let (dest,f) = destForRaw s d fpath
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
destForShrink maxwidth =
  destFor (\f -> (takeFileName (dropExtension f)) ++ "." ++ (show maxwidth) ++ ".png")

destFor :: (FilePath -> FilePath) -> FilePath -> FilePath -> FilePath -> (FilePath, FilePath)
destFor fNameMaker src dest fileInSrc =
  let srel = takeDirectory $ makeRelative src fileInSrc
      fName = fNameMaker fileInSrc
  in (dest </> srel </> fName, fName)

shrink :: Int -> Int -> Int -> (Int, Int)
shrink maxwidth w h = let factor = fromIntegral maxwidth / fromIntegral w
                          scale x = floor ((fromIntegral x) * factor)
                      in (scale w, scale h)


--
-- file/directory utilities
--

dirsOnly :: [FilePath] -> IO [FilePath]
dirsOnly = filterM doesDirectoryExist

readLink :: FilePath -> IO FilePath
readLink f = do
  isLink <- pathIsSymbolicLink f
  if isLink then do
    target <- getSymbolicLinkTarget f
    if isAbsolute target then do
      readLink target
    else do
      let ftgt = takeDirectory f </> target
      readLink ftgt
  else do
    return f

--
-- I/O utilities
--

putStrSameLn :: String -> IO ()
putStrSameLn s = do
    putStr "\r                                                                                                                                                          "
    putStr "\r"
    putStr s
    hFlush stdout

--
-- basic utilities
--

maybeTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
maybeTuple (ma, mb) =
  case ma of
    Just a ->
      case mb of
        Just b ->
          Just (a, b)
        Nothing ->
          Nothing
    Nothing ->
      Nothing
