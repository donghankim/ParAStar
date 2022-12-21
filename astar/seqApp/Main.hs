module Main (main) where

import SeqLib

main :: IO ()
main = do 
  path <- seqSearch
  writeFile "res.txt" $ unlines (map show path)



