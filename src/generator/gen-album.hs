{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

import AlbumTypes
import Codec.Picture hiding (Image)
import Codec.Picture.Metadata
import qualified Codec.Picture.Types
import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Control.Parallel.Strategies
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import Data.List (find, intercalate, sort)
import Data.Maybe
import Data.Time.Clock
import Data.Tuple
import Graphics.Image (Bilinear (Bilinear), Border (Fill), resize, fromJPImageRGB8, toJPImageRGB8)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Files
import Text.Regex

--
-- main & usage
--

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, dest] -> writeAlbumOrList src dest
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "usage: gen-album <src> <dest>"
  exitWith $ ExitFailure 1

--
-- configuration
--

thumbFilename :: String
thumbFilename = "thumbnail"

sizes :: [Int]
sizes = [1600, 1400, 1200, 1000, 800, 400, 200]

--
-- Album & AlbumList functions
--

writeAlbumOrList :: String -> String -> IO ()
writeAlbumOrList src dest = do
  existingAlbumData <- findExistingAlbumData dest "album.json"
  case existingAlbumData of
    Nothing ->
      putStrLn "album metadata not found; all image metadata will be read from source"
    Just (_, modDate) ->
      putStrLn $ "album metadata dated " ++ show modDate ++ " found; will use it for source images modified earlier than this"
  putStrLn ""
  eAlbumOrList <- genAlbumOrList src src dest existingAlbumData ChooseAutomatically
  case eAlbumOrList of
    Left errs -> do
      putStrLn ""
      putStrLn $ intercalate "\n" errs
      exitWith $ ExitFailure 1
    Right albumOrList ->
      C.writeFile (dest </> "album.json") $ encode albumOrList

findExistingAlbumData :: FilePath -> String -> IO (Maybe (AlbumOrList, UTCTime))
findExistingAlbumData d albumFile = do
  exists <- doesFileExist $ d </> albumFile
  case exists of
    False -> return Nothing
    True -> do
      bytes <- C.readFile $ d </> albumFile
      date <- getModificationTime $ d </> albumFile
      return $ swap . (,) date <$> decode bytes

data ThumbnailSpec
  = ChooseAutomatically
  | RequireExplicit

genAlbumOrList :: FilePath -> FilePath -> FilePath -> Maybe (AlbumOrList, UTCTime) -> ThumbnailSpec -> IO (Either [String] AlbumOrList)
genAlbumOrList srcRoot src dest existingAlbumData thumbspec = do
  files <- filter (`notElem` [".", "..", thumbFilename]) <$> getDirectoryContents src
  let afiles = map (src </>) (sort files)
  epimgs <- procImgsOnly srcRoot src dest existingAlbumData afiles
  case epimgs of
    Left e ->
      return $ Left e
    Right pimgs -> do
      subdirs <- dirsOnly afiles
      let icount = length pimgs
          idirs = length subdirs
      if (icount > 0) && (idirs > 0)
        then return $ Left ["directory " ++ src ++ " contains both images and subdirs, this is not supported"]
        else case pimgs of
          imgFirst : imgRest -> do
            aOrErr <- genAlbum srcRoot src dest imgFirst imgRest
            case aOrErr of
              Left err ->
                return $ Left [err]
              Right a ->
                return $ Right $ Leaf a
          [] ->
            case subdirs of
              dirFirst : dirRest -> do
                en <- genNode srcRoot src dest existingAlbumData thumbspec dirFirst dirRest
                case en of
                  Left err ->
                    return $ Left err
                  Right n ->
                    return $ Right $ List n
              [] ->
                return $ Left ["no images or subdirs in " ++ src]

genNode :: FilePath -> FilePath -> FilePath -> Maybe (AlbumOrList, UTCTime) -> ThumbnailSpec -> FilePath -> [FilePath] -> IO (Either [String] AlbumList)
genNode srcRoot src dest existingAlbumData thumbspec dirFirst dirRest = do
  ecFirst <- genAlbumOrList srcRoot dirFirst dest existingAlbumData RequireExplicit
  case ecFirst of
    Left err ->
      return $ Left err
    Right cFirst -> do
      ecRest <- mapM (\dir -> genAlbumOrList srcRoot dir dest existingAlbumData RequireExplicit) dirRest
      case lefts ecRest of
        [] -> do
          let cRest = rights ecRest
              childImages = getChildImages $ cFirst : cRest
          thumbOrErr <- findThumb srcRoot src dest childImages
          case thumbOrErr of
            Left err ->
              case thumbspec of
                ChooseAutomatically ->
                  case childImages of
                    [] ->
                      return $ Left ["cannot automatically chose a thumbnail when there are no subalbums " ++ titleForDir src]
                    firstChild : _ ->
                      return $
                        Right $
                          AlbumList
                            { listTitle = titleForDir src,
                              listThumbnail = firstChild,
                              childFirst = cFirst,
                              childRest = cRest
                            }
                RequireExplicit ->
                  return $ Left [err]
            Right thumb ->
              return $
                Right $
                  AlbumList
                    { listTitle = titleForDir src,
                      listThumbnail = thumb,
                      childFirst = cFirst,
                      childRest = cRest
                    }
        errs ->
          return $ Left $ concat errs

getChildImages :: [AlbumOrList] -> [Image]
getChildImages =
  concatMap getChildImages1

getChildImages1 :: AlbumOrList -> [Image]
getChildImages1 albumOrList =
  case albumOrList of
    List albumTreeNode ->
      getChildImages $ childFirst albumTreeNode : childRest albumTreeNode
    Leaf album ->
      imageFirst album : imageRest album

genAlbum :: FilePath -> FilePath -> FilePath -> Image -> [Image] -> IO (Either String Album)
genAlbum srcRoot src dest imgFirst imgRest = do
  thumbOrErr <- findThumb srcRoot src dest $ imgFirst : imgRest
  case thumbOrErr of
    Left err ->
      return $ Left err
    Right thumb ->
      return $
        Right $
          Album
            { title = titleForDir src,
              thumbnail = thumb,
              imageFirst = imgFirst,
              imageRest = imgRest
            }

titleForDir :: String -> String
titleForDir dir =
  let dirName = last $ splitDirectories dir
   in subRegex (mkRegex "^[0-9]+_") dirName ""

findThumb :: FilePath -> FilePath -> FilePath -> [Image] -> IO (Either String Image)
findThumb srcRoot src dest images = do
  let thumbLink = src </> thumbFilename
  thumbLinkFileExists <- doesFileExist thumbLink
  if thumbLinkFileExists
    then do
      thumbLinkExists <- pathIsSymbolicLink thumbLink
      if thumbLinkExists
        then do
          thumbPath <- readLink thumbLink
          if isAbsolute $ makeRelative srcRoot thumbPath
            then return $ Left $ src ++ " thumbnail link must point to a path inside " ++ srcRoot ++ ", but does not: " ++ thumbPath
            else do
              let thumbDest = fst $ destForRaw srcRoot dest thumbPath
                  isThumb i = equalFilePath thumbDest (dest </> url (srcSetFirst i))
                  thumb = find isThumb images
              case thumb of
                Nothing ->
                  return $ Left $ src ++ " thumbnail '" ++ thumbPath ++ "' (" ++ thumbDest ++ ") does not point to any images in this album: " ++ concatMap (\i -> dest </> url (srcSetFirst i)) images
                Just t ->
                  return $ Right t
        else return $ Left $ thumbLink ++ " is not a symbolic link"
    else return $ Left $ src ++ " does not contain a 'thumbnail' file"

--
-- Single-image functions
--

data FileClassification
  = NotAnImage
  | AlreadyProcessed Image
  | ToProcess (DynamicImage, Metadatas)

toProc :: FileClassification -> Bool
toProc (ToProcess _) = True
toProc _ = False

procImgsOnly :: FilePath -> FilePath -> FilePath -> Maybe (AlbumOrList, UTCTime) -> [FilePath] -> IO (Either [String] [Image])
procImgsOnly _ _ _ _ [] = return $ Right []
procImgsOnly srcRoot src dest existingAlbumData files = do
  let classify f = do
        classification <- classifyFile srcRoot dest existingAlbumData f
        return (f, classification)
  putStrSameLn $ src ++ ": classifying " ++ show (length files) ++ " files ... "
  classifications <- mapConcurrently classify files
  let tpCt = show $ length $ filter toProc $ map snd classifications
  putStr $ tpCt ++ " to process"
  productionResults <- mapConcurrently (produceImage srcRoot dest) classifications
  case lefts productionResults of
    [] ->
      return $ Right $ catMaybes $ rights productionResults
    errs ->
      return $ Left errs

produceImage :: FilePath -> FilePath -> (FilePath, FileClassification) -> IO (Either String (Maybe Image))
produceImage srcRoot dest (f, classification) = do
  case classification of
    NotAnImage ->
      return $ Right Nothing
    AlreadyProcessed img ->
      return $ Right $ Just img
    ToProcess metadata -> do
      eImg <- procImage srcRoot dest (f, metadata)
      case eImg of
        Left err ->
          return $ Left err
        Right img ->
          return $ Right $ Just img

classifyFile :: FilePath -> FilePath -> Maybe (AlbumOrList, UTCTime) -> FilePath -> IO FileClassification
classifyFile srcRoot destDir existingAlbumData file = do
  mImg <- alreadyProcessed srcRoot destDir existingAlbumData file
  case mImg of
    Just img ->
      return $ AlreadyProcessed img
    Nothing -> do
      mMetadata <- imgOnly file
      case mMetadata of
        Just metadata ->
          return $ ToProcess $ snd metadata
        Nothing ->
          return NotAnImage

alreadyProcessed :: FilePath -> FilePath -> Maybe (AlbumOrList, UTCTime) -> FilePath -> IO (Maybe Image)
alreadyProcessed s d existingAlbumData f = do
  let rawDest = fst $ destForRaw s d f
      allDests = rawDest : map snd (shrinkDests s d f)
      existingImage = (>>=) existingAlbumData (matchExisting s f . fst)
  --putStrLn $ "alreadyProcessed " ++ s ++ ", " ++ f ++ ": " ++ (show existingImage)
  destsExist <- mapM doesFileExist allDests
  if and destsExist
    then do
      let existingImageAndModDate = liftA2 (,) existingImage $ fmap snd existingAlbumData
      srcModTimeOrNewerImage <- imageNewerThanSrc existingImageAndModDate f
      case srcModTimeOrNewerImage of
        Right newerImage ->
          return $ Just newerImage
        Left srcModTime -> do
          mi <- imgOnly f
          case mi of
            Nothing ->
              return Nothing
            Just i -> do
              -- the destination images all exist
              -- the source image exists, but is newer than the metadata we have from album.json
              -- the source image also loads cleanly and we have its intrinsic metadata
              -- however, it's only safe to use that instrinsic metadata if the destination images
              -- are all *newer* than the source image
              destModTimes <- mapM getModificationTime allDests
              case maximum destModTimes > srcModTime of
                True -> createImageWithMetadataSize s d f rawDest i
                False -> return Nothing
    else return Nothing

shrinkDests :: FilePath -> FilePath -> FilePath -> [(Int, FilePath)]
shrinkDests s d f =
  map
    ( \maxwidth ->
        ( maxwidth,
          fst $ destForShrink maxwidth s d f
        )
    )
    sizes

matchExisting :: FilePath -> FilePath -> AlbumOrList -> Maybe Image
matchExisting s f albumOrList = do
  let relNames = splitDirectories $ makeRelative s f
   in findImage relNames albumOrList

findImage :: [String] -> AlbumOrList -> Maybe Image
findImage [] _ = Nothing
findImage (name : names) albumOrList =
  let getTitle aol =
        case aol of
          List list -> listTitle list
          Leaf album -> title album
      matchesName = (==) (titleForDir name) . getTitle
   in case albumOrList of
        List list ->
          case filter matchesName $ childFirst list : childRest list of
            [] -> Nothing
            _ : _ : _ -> Nothing
            [child] -> findImage names child
        Leaf album ->
          case filter ((==) (takeBaseName name) . altText) $ imageFirst album : imageRest album of
            [] -> Nothing
            _ : _ : _ -> Nothing
            [child] -> Just child

imageNewerThanSrc :: Maybe (Image, UTCTime) -> FilePath -> IO (Either UTCTime Image)
imageNewerThanSrc mImageModTime src = do
  srcModTime <- getModificationTime src
  case mImageModTime of
    Nothing ->
      return $ Left srcModTime
    Just (image, modTime) ->
      case modTime >= srcModTime of
        True ->
          --putStrLn $ "found image in album metadata @ " ++ (show modTime) ++ " newer than src " ++ src ++ " @ " ++ (show srcModTime) ++ ": " ++ (show image)
          return $ Right image
        False ->
          --putStrLn $ "found image in album metadata @ " ++ (show modTime) ++ " older than src " ++ src ++ " @ " ++ (show srcModTime) ++ ": " ++ (show image)
          return $ Left srcModTime

createImageWithMetadataSize :: FilePath -> FilePath -> FilePath -> FilePath -> (FilePath, (DynamicImage, Metadatas)) -> IO (Maybe Image)
createImageWithMetadataSize s d f rawDest i = do
  let mw = Codec.Picture.Metadata.lookup Width $ snd $ snd i
      mh = Codec.Picture.Metadata.lookup Height $ snd $ snd i
      wh = maybeTuple (mw, mh)
  case wh of
    Just (ww, hw) -> do
      let t = takeBaseName f
          w = fromIntegral ww
          h = fromIntegral hw
          srcSetFst =
            ImgSrc
              { url = makeRelative d rawDest,
                x = w,
                y = h
              }
          sdToImgSrc sd =
            let (xsm, ysm) = shrink (fst sd) w h
             in ImgSrc
                  { url = makeRelative d $ snd sd,
                    x = xsm,
                    y = ysm
                  }
          srcSetRst = map sdToImgSrc $ shrinkDests s d f
      return $
        Just $
          Image
            { altText = t,
              srcSetFirst = srcSetFst,
              srcSetRest = srcSetRst
            }
    Nothing ->
      return Nothing

imgOnly :: FilePath -> IO (Maybe (FilePath, (DynamicImage, Metadatas)))
imgOnly f = do
  loadResult <- readImageWithMetadata f
  case loadResult of
    Left _ -> return Nothing
    Right img ->
      do
        --putStrSameLn $ "loaded " ++ show f
        return $ Just (f, img)

procImage :: FilePath -> FilePath -> (FilePath, (DynamicImage, Metadatas)) -> IO (Either String Image)
procImage s d (f, i) = do
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
      if fromIntegral ww == w && fromIntegral hh == h
        then do
          (srcSetFst, srcSetRst) <- procSrcSet s d f (fst i) w h
          return $
            Right $
              Image
                { altText = t,
                  srcSetFirst = srcSetFst,
                  srcSetRest = srcSetRst
                }
        else return $ Left $ "image " ++ f ++ " has intrinsic w*h of " ++ show w ++ "*" ++ show h ++ " but metadata w*h of " ++ show ww ++ "*" ++ show hh ++ "; to repair, load in The Gimp, then choose file -> overwrite"

procSrcSet :: FilePath -> FilePath -> FilePath -> DynamicImage -> Int -> Int -> IO (ImgSrc, [ImgSrc])
procSrcSet s d f i w h = do
  let shrunkenSrcs = map (shrinkImgSrc s d f i w h) sizes `using` parList rdeepseq
      shrunken = map fth shrunkenSrcs
  rawImg <- copyRawImgSrc s d f w h
  --putStrSameLn $ "processing " ++ show f ++ " "
  mapM_ (writeShrunkenImgSrc . fstSndThr) shrunkenSrcs
  return (rawImg, shrunken)

writeShrunkenImgSrc :: (Codec.Picture.Types.Image PixelRGB8, FilePath, Int) -> IO ()
writeShrunkenImgSrc (ism, fsmpath, _) = do
  createDirectoryIfMissing True $ takeDirectory fsmpath
  --putStr $ show maxwidth ++ "w "
  hFlush stdout
  savePngImage fsmpath $ ImageRGB8 ism

shrinkImgSrc :: FilePath -> FilePath -> FilePath -> DynamicImage -> Int -> Int -> Int -> (Codec.Picture.Types.Image PixelRGB8, FilePath, Int, ImgSrc)
shrinkImgSrc s d f i w h maxwidth =
  let (xsm, ysm) = shrink maxwidth w h
      fsmpath = fst $ destForShrink maxwidth s d f
      hipImg = fromJPImageRGB8 $ convertRGB8 i
      hipImgSmall = resize Bilinear (Fill 0) (ysm, xsm) hipImg
      ism = toJPImageRGB8 hipImgSmall
   in ( ism,
        fsmpath,
        maxwidth,
        ImgSrc
          { url = makeRelative d fsmpath,
            x = xsm,
            y = ysm
          }
      )

copyRawImgSrc :: FilePath -> FilePath -> FilePath -> Int -> Int -> IO ImgSrc
copyRawImgSrc s d fpath w h = do
  let (dest, _) = destForRaw s d fpath
  createDirectoryIfMissing True $ takeDirectory dest
  copyFile fpath dest
  setFileMode dest $ foldl unionFileModes ownerReadMode [groupReadMode, otherReadMode]
  --putStrSameLn $ "copied " ++ f
  return
    ImgSrc
      { url = makeRelative d dest,
        x = w,
        y = h
      }

destForRaw :: FilePath -> FilePath -> FilePath -> (FilePath, FilePath)
destForRaw =
  destFor takeFileName

destForShrink :: Int -> FilePath -> FilePath -> FilePath -> (FilePath, FilePath)
destForShrink maxwidth =
  destFor (\f -> takeFileName (dropExtension f) ++ "." ++ show maxwidth ++ ".png")

destFor :: (FilePath -> FilePath) -> FilePath -> FilePath -> FilePath -> (FilePath, FilePath)
destFor fNameMaker src dest fileInSrc =
  let srel = takeDirectory $ makeRelative src fileInSrc
      fName = fNameMaker fileInSrc
   in (dest </> srel </> fName, fName)

shrink :: Int -> Int -> Int -> (Int, Int)
shrink maxwidth w h =
  let factor = fromIntegral maxwidth / fromIntegral w
      scale width = floor (fromIntegral width * factor)
   in (scale w, scale h)

--
-- file/directory utilities
--

dirsOnly :: [FilePath] -> IO [FilePath]
dirsOnly = filterM doesDirectoryExist

readLink :: FilePath -> IO FilePath
readLink f = do
  isLink <- pathIsSymbolicLink f
  if isLink
    then do
      target <- getSymbolicLinkTarget f
      if isAbsolute target
        then readLink target
        else do
          let ftgt = takeDirectory f </> target
          readLink ftgt
    else return f

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

fstSndThr :: (a, b, c, d) -> (a, b, c)
fstSndThr (a, b, c, _) =
  (a, b, c)

fth :: (a, b, c, d) -> d
fth (_, _, _, d) =
  d
