module Main (main) where


import System.IO
import SeqLib

main :: IO ()
main = do
  putStrLn "*** Sequential A-Star Path Finder ***"
  graphPath <-  putStr "Enter Graph .txt File: " *> hFlush stdout *> getLine
  fh <- openFile graphPath ReadMode
  loadFile fh

  sourceIdx <- putStr "Enter Start Node Index: " *> hFlush stdout *> getLine
  targetIdx <- putStr "Enter Destination Node Index: " *> hFlush stdout *> getLine
  putStrLn "good exit"



