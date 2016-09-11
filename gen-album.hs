import System.Directory
import System.Environment

main = do
  args <- getArgs
  case args of
       [] -> usage
       _:[] -> usage
       src:dest:[] -> genAlbum src dest
       _:_:_:_ -> usage

usage = putStrLn "usage: gen-album <src> <dest>"

genAlbum src dest = do
  ls <- getDirectoryContents src
  putStrLn ((show (length ls)) ++ " files in " ++ src ++ " to copy to " ++ dest)
  