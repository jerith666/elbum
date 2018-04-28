import System.Environment
import System.IO
import System.Exit

import System.Directory
import System.FilePath
import System.Posix.Files

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
       src:[] -> writeAlbumOrList src
       _ -> usage

usage = do
  putStrLn "usage: check-pics <src>"
  exitWith $ ExitFailure 1

--
-- configuration
--

thumbFilename = "thumbnail"

--
-- Album & AlbumList functions
--

writeAlbumOrList :: String -> IO ()
writeAlbumOrList src = do
  eAlbumOrList <- genAlbumOrList src src True
  case eAlbumOrList of
    Left err -> do
      putStrLn ""
      putStrLn err
      exitWith $ ExitFailure 1
    Right albumOrList ->
      putStrLn "all images are okay"

genAlbumOrList :: FilePath -> FilePath -> Bool -> IO (Either String ())
genAlbumOrList srcRoot src autoThumb = do
  files <- filter (`notElem` [".","..",thumbFilename]) <$> getDirectoryContents src
  let afiles = map (\f -> src </> f) (sort files)
  epimgs <- procImgsOnly srcRoot afiles
  case epimgs of
    Left e -> do
      return $ Left e
    Right _ -> do
      subdirs <- dirsOnly afiles
      if length subdirs > 0 then do
        en <- genNode srcRoot src autoThumb subdirs
        case en of
          Left err ->
            return $ Left $ err
          Right _ ->
            return $ Right ()
      else do
        return $ Right ()

genNode :: FilePath -> FilePath -> Bool -> [FilePath] -> IO (Either String ())
genNode srcRoot src autoThumb dirs = do
  ecFirst <- genAlbumOrList srcRoot (head dirs) False
  case ecFirst of
    Left err ->
      return $ Left $ err
    Right cFirst -> do
      ecRest <- mapM (\dir -> genAlbumOrList srcRoot dir False) (tail dirs)
      case lefts ecRest of
        [] -> do
          return $ Right ()
        errs -> do
          return $ Left $ concat $ intersperse "\n" errs

--
-- Single-image functions
--

procImgsOnly :: FilePath -> [FilePath] -> IO (Either String ())
procImgsOnly _ [] = return $ Right ()
procImgsOnly s (f:fs) = do
  mi <- imgOnly f
  case mi of
    Nothing -> do
      is <- procImgsOnly s fs
      return is
    Just pdi -> do
      -- this ordering is key to ensuring memory usage remains relatively constant
      -- we have to process the first image completely (load it, save it out at all
      -- sizes, and convert to an Image) before moving on to the others
      epi <- procImage s pdi
      eis <- procImgsOnly s fs
      case epi of
        Left e -> do
          case eis of
            Left e2 -> do
              return $ Left $ e ++ "\n" ++ e2
            Right _ -> do
              return $ Left $ e
        Right pi -> do
          case eis of
            Left e ->
              return $ Left e
            Right _ ->
              return $ Right $ ()

imgOnly :: FilePath -> IO (Maybe (FilePath, (DynamicImage, Metadatas)))
imgOnly f = do
    loadResult <- readImageWithMetadata f
    case loadResult of
         Left err -> do return Nothing
         Right img ->
             do
                 putStrSameLn $ "checking " ++ (show f)
                 return $ Just (f, img)

procImage :: FilePath -> (FilePath, (DynamicImage, Metadatas)) -> IO (Either String ())
procImage s (f,i) = do
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
          return $ Right ()
        else do
          return $ Left $ "image " ++ f ++ " has intrinsic w*h of " ++ (show w) ++ "*" ++ (show h) ++ " but metadata w*h of " ++ (show ww) ++ "*" ++ (show hh) ++ "; to repair, load in The Gimp, then choose file -> overwrite"


--
-- file/directory utilities
--

dirsOnly :: [FilePath] -> IO [FilePath]
dirsOnly = filterM doesDirectoryExist


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
