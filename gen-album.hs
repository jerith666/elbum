import System.Directory

main = do
  cd <- getCurrentDirectory
  ls <- getDirectoryContents cd
  putStrLn ((show (length ls)) ++ " files in " ++ (show cd))
  