module SeqInput
    ( loadFile
    ) where

{-# LANGUAGE OverloadedStrings #-}

import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read 
import Data.Either


data Node = Node { key :: Int,
                   coord :: (Double, Double),
                   edges :: Maybe [(Int, Float)]
                   } deriving (Show)


foo :: T.Text -> Int
foo text = fromRight 0 text

extractKey :: T.Text -> Int
extractKey key = fromJust (readMaybe $ T.unpack key)

extractCoord :: T.Text -> (Double, Double)
extractCoord coord = (y, x)
  where
    [yt,xt] = T.splitOn "&" coord
    y = fromJust (readMaybe $ T.unpack yt)
    x = fromJust (readMaybe $ T.unpack xt)

extractEdge :: T.Text -> (Int, Float)
extractEdge edge = (n,w)
  where
    [nt, wt] = T.splitOn ";" edge
    n = fromJust (readMaybe $ T.unpack)
    w = fromJust (readMaybe $ T.unpack)

parseLine :: T.Text -> [(Int, Float)]
parseLine tstr = nCoord
  where
    [idx, info] = T.splitOn ":" tstr
    [coord, edges] = T.splitOn "*" info 
    nKey = extractKey idx
    nCoord = extractCoord coord
    edgeList = T.splitOn "," edges
    -- nEdge :: [(Int, Float)]
    -- nEdge = fmap (\x -> T.splitOn ";") edgeList

-- load graph data
loadFile :: Handle -> IO ()
loadFile fh = do
  eof <- hIsEOF fh
  if eof
  then hClose fh 
  else do
    tstr <- TIO.hGetLine fh 
    let aa = parseLine tstr
    putStrLn $ show aa
    loadFile fh



{- notes

# creating a new node
n1 = Node {idx = 0, coord = (0.454, 0.2121), edges = Nothing}
n2 = Node {idx = 1, coord = (0.454, 0.2121), edges = Just [(0, 3.3)]}
createNode :: String -> Node
createNode str = Node {idx = i, coord = (x,y), edges = edge_arr}
  where
    i = str

copyDo :: IO ()
copyDo = do
  putStr "Enter repretition: "
  n <- getLine
  putStr "Enter phrase: "
  word <- getLine
  replicateM_ (read n) (putStrLn word)


# for cleaner file processing
import Text.Read (readMaybe)

data Participant = Participant { name :: String, age :: Int, country :: String }

parseParticipants :: String -> [Participant]
parseParticipants fileContents = do
    [name', age', country'] <- words <$> lines fileContents
    Just age'' <- return (readMaybe age')
    return (Participant { name = name', age = age'', country = country' })

main :: IO ()
main = do
    participants <- parseParticipants <$> readFile "yourfile.txt"
    -- do things with the participants
    return ()

-}
