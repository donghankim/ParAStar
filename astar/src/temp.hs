{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Data.PQueue.Min as P

import Data.Text.Read
import Data.Either
import Data.Maybe (fromJust, isNothing)

-- return ([202, 90, 31, 86, 82, 72])

data Node = Node { idx   :: Int,
                   coord :: (Double, Double),
                   edges :: [(Int, Double)]
                   } deriving (Show)


aa = Node {idx = 0, coord = (1,1), edges = []}
bb = Node {idx = 1, coord = (2,2), edges = []}
cc = Node {idx = 2, coord = (3,3), edges = []}

nodeMap = M.fromList [(0, aa), (1, bb), (2, cc)]




{-

-- chat parllelizing IO
The "L" refers to Data.ByteString.Lazy
One way to achieve this in Haskell is to use the parallel package. This package provides a parMap function,
which allows you to perform a mapping operation in parallel by distributing the elements of the input list across a number of parallel threads.

Here's an example of how you could use parMap to load a large text file in parallel:

import Control.Parallel.Strategies

-- | Read a file in chunks, using a given number of threads
readFileInChunks :: Int -> FilePath -> IO [String]
readFileInChunks numThreads filePath = do
  -- Open the file and read its contents into a lazy ByteString
  lbs <- L.readFile filePath

  -- Split the ByteString into chunks of equal size
  let chunkSize = fromIntegral (L.length lbs `div` numThreads)
      chunks = L.chunksOf chunkSize lbs

  -- Use parMap to apply the 'L.toStrict' function to each chunk in parallel
  -- The 'rdeepseq' function ensures that the result of the mapping operation
  -- is fully evaluated before the main thread continues
  let result = parMap rdeepseq L.toStrict chunks
  return result

This function takes a file path and a number of threads as input, and returns a list of Strings representing the chunks of the file that were read in parallel.
To use this function, you would call it like this:

chunks <- readFileInChunks numThreads filePath

Note that the parMap function is provided by the Control.Parallel.Strategies module, which is part of the parallel package.
In order to use this function, you will need to add a dependency on the parallel package in your project's .cabal file.

-}
