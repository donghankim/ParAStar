{-# LANGUAGE OverloadedStrings #-}

module SeqLib
    ( loadFile
    ) where

import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read 
import Data.Either


data Node = Node { key :: Int,
                   coord :: (Double, Double),
                   edges :: [(Int, Double)]
                   } deriving (Show)


extractInt :: Either String (Int, T.Text) -> Int
extractInt val
  | isRight val = fst $ fromRight (0, "") val
  | otherwise = error "[.txt data] Node index is corrupted..."

extractDouble :: Either String (Double, T.Text) -> Double
extractDouble val
  | isRight val = fst $ fromRight (0, "") val
  | otherwise = error "[.txt data] Node long/lat is corrupted..."


parseLine :: T.Text -> Node
parseLine tStr = node
  where
    [idx, info] = T.splitOn ":" tStr
    [coord, edges] = T.splitOn "*" info
    [yt, xt] = T.splitOn "&" coord 
    edgeList = T.splitOn "," edges
    nEdges = [(id, wt) | dt <- edgeList, 
                        let [idVal, wVal] = T.splitOn ";" dt,
                        let id = extractInt $ decimal idVal,
                        let wt = extractDouble $ rational wVal]
    
    nKey = extractInt $ decimal idx
    nCoord = (extractDouble $ rational yt, extractDouble $ rational xt)
    node = Node {key = nKey, coord = nCoord, edges = nEdges}
    
  
-- load graph data
loadFile :: Handle -> IO ()
loadFile fh = do
  eof <- hIsEOF fh
  if eof
  then hClose fh 
  else do
    tstr <- TIO.hGetLine fh 
    let newNode = parseLine tstr
    putStrLn (show $ coord newNode)
    loadFile fh



