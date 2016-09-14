import System.Environment

import System.Directory
import System.FilePath

import Codec.Picture

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
  putStrLn $ (show (length files)) ++ " files in " ++ src
  let afiles = map (\f -> src </> f) files
  putStrLn $ "first file: " ++ (head afiles)
  imgs <- imgsOnly afiles
  -- sequence $ map procImage $ imgs
  putStrLn ((show (length imgs)) ++ " imgs in " ++ src ++ " to copy to " ++ dest)
  putStrLn $ "first img is " ++ (show $ head imgs)

imgsOnly :: [FilePath] -> IO [FilePath]
imgsOnly [] = return []
imgsOnly (f:fs) = do fo <- imgOnly f
                     fos <- imgsOnly fs
                     return $ fo ++ fos

imgOnly :: FilePath -> IO [FilePath]
imgOnly f = do loadResult <- readImage f
               case loadResult of
                    Left err -> do putStrLn $ "error loading " ++ f ++ ": " ++ err
                                   return []
                    Right img -> do putStrLn $ "loaded " ++ f
                                    return [f]
