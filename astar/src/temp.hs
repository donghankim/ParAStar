{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read 
import Data.Either
import Text.Read (readMaybe)
import Data.Maybe


extractKey :: T.Text -> Double 
extractKey key = fst $ fromRight (-1, "") (rational key)

aa = T.pack "10"

foo :: Int -> Int
foo x
  | x > 0 = 10
  | otherwise = error "data error"


extractInt :: Either String (Int, T.Text) -> Int
extractInt val
  | isRight val = fst $ fromRight (0, "") val
  | otherwise = error "[.txt data] Node index is corrupted..."

{- notes


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


extractCoord coord = (y, x)
  where
    [yt,xt] = T.splitOn "&" coord
    y = fst $ fromRight (-1, "") (rational yt)
    x = fst $ fromRight (-1, "") (rational xt)
  
extractEdge :: T.Text -> (Int, Float)
extractEdge edge = (n,w)
  where
    [idx, wt] = T.splitOn ";" edge
    n = fst $ fromRight (-1, "") (decimal idx)
    w = fst $ fromRight (-1, "") (rational wt)






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

data Participant = Participant { name :: String, age :: Int, country :: String }


-}
