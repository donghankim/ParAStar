module Main (main) where

import Lib

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Data.PQueue.Min as P
import Data.Maybe (fromJust)

main :: IO ()
main = do
  putStrLn "*** Parallel A-Star Path Finder ***"
  graphFile <-  putStr "Enter Graph .txt File: " *> hFlush stdout *> getLine
  fp <- openFile ("data/" ++ graphFile) ReadMode
  content <- TIO.hGetContents fp
  start <- putStr "Enter Start Node Index: " *> hFlush stdout *> getLine
  target <- putStr "Enter Destination Node Index: " *> hFlush stdout *> getLine

  let nodeMap = M.fromList $ map (parseLine) (T.lines content)
      sIdx = read start :: Int
      tIdx = read target :: Int
      openList = P.singleton (0.0, sIdx) :: P.MinQueue (Double, Int)
      closedList = S.empty
      cameFrom = M.empty :: M.IntMap Int
      path = parAstar sIdx tIdx nodeMap openList closedList cameFrom

  case path of
       Nothing -> putStrLn "No path found..."
       _       -> writeFile "res.txt" $ unlines (map show $ fromJust path)

  putStrLn "A* path written to res.txt! Use graph.py to plot the shortest path."
