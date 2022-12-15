module Main (main) where

{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import qualified Data.ByteString as T
-- import Text.Regex.Posix
import SeqInput
import System.IO
import Control.Monad (replicateM_)


data Node = Node { key :: Int,
                   coord :: (Float, Float),
                   edges :: Maybe [(Int, Float)]
                   } deriving (Show)


-- parse line
parseLine :: T.Text -> IO ()
parseLine tstr = TIO.putStrLn key
  where
  node = "temp"
  tmp = T.splitOn (T.pack ":") tstr
  key = head tmp
  info = tail tmp
  -- split info into coord and edges


-- load graph data
loadFile :: Handle -> IO ()
loadFile fh = do
  eof <- hIsEOF fh
  if eof
  then putStrLn "Done Processing"
  else do
    tstr <- TIO.hGetLine fh 
    parseLine tstr
    loadFile fh
      

main :: IO ()
main = do 
  putStrLn "Enter graph data .txt file: "
  graphPath <- getLine
  fh <- openFile graphPath ReadMode
  loadFile fh
  hClose fh



{- notes

# creating a new node
n1 = Node {idx = 0, coord = (0.454, 0.2121), edges = Nothing}
n2 = Node {idx = 1, coord = (0.454, 0.2121), edges = Just [(0, 3.3)]}
createNode :: String -> Node
createNode str = Node {idx = i, coord = (x,y), edges = edge_arr}
  where
    i = str

# fo notation example
copyPrompt :: IO()
copyPrompt = 
  putStr "Enter repetition: " >>
  getLine >>= \n -> 
  putStr "Enter phrase: " >>
  getLine >>= \word ->
  replicateM_ (read n) (putStrLn word)

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
