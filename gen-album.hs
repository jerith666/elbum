import System.Directory
import System.Environment
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
  imgs <- imgsOnly files
  -- sequence $ map procImage $ imgs
  putStrLn ((show (length imgs)) ++ " imgs in " ++ src ++ " to copy to " ++ dest)

imgsOnly :: [FilePath] -> IO [FilePath]
imgsOnly [] = return []
imgsOnly (f:fs) = do fo <- imgOnly f
                     fos <- imgsOnly fs
                     return $ fo ++ fos

imgOnly :: FilePath -> IO [FilePath]
imgOnly f = do loadResult <- readImage f
               case loadResult of
                    Left err -> return []
                    Right img -> return [f]
