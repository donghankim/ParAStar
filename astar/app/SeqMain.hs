module SeqMain (main) where


import System.IO
import SeqInput

main :: IO ()
main = do 
  putStr "Enter Graph .txt File: "
  graphPath <- getLine
  fh <- openFile graphPath ReadMode
  loadFile fh
  putStr "Enter Source Node idx: "
  source <- getLine
  putStr "Enter Destination Node idx: "
  target <- getLine
  putStrLn source ++ target




