{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntSet as S
import qualified Data.IntMap as M
-- import qualified Data.PQueue.Prio.Min as PQ
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
A* pseudo code: openList and closedSet can be modified!

currNode = getMin openList
if currNode == targetNode:
  done!
elif currNode in closedSet:
  continue
else:
  get adjNode = edges currNode


  for node in adjNode:
    if ni in closedSet:
      continue
    else:
      fn = g(ni) + h(ni)
      if fn < openList[ni]:
        update openList
        update cameFrom ni -> currNode
      else:
        add ni to openList
        add ni to cameFrom

  add currNode to closedSet
-}


{-
-- chat a* search
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.PQueue.Prio as PQ

astar :: Node -> Node -> [Int]
astar sourceNode targetNode = search IM.empty (PQ.singleton (heuristic sourceNode targetNode) sourceNode) IS.empty
  where
    search cameFrom openSet closedSet
      | PQ.null openSet = [] -- No path found
      | currentNode == targetNode = reconstructPath cameFrom targetNode -- Path found, reconstruct and return it
      | otherwise = search updatedCameFrom updatedOpenSet updatedClosedSet
      where
        currentNode = PQ.findMin openSet
        neighbors = edges currentNode
        (updatedCameFrom, updatedOpenSet, updatedClosedSet) = foldl' updateNeighbor (cameFrom, openSet, closedSet) neighbors
        updateNeighbor (cameFrom', openSet', closedSet') (neighborIdx, gCost)
          | neighborIdx `IS.member` closedSet' = (cameFrom', openSet', closedSet') -- Skip this neighbor if it's already been processed
          | otherwise =
              let neighborNode = nodeMap IM.! neighborIdx
                  tentativeGScore = gCost + (fst . coord $ currentNode)
                  neighborGScore = (fst . coord $ neighborNode)
                  fScore = tentativeGScore + heuristic neighborNode targetNode
              in if tentativeGScore >= neighborGScore
                   then (cameFrom', openSet', closedSet') -- This path is not a better one, skip it
                   else (IM.insert neighborIdx currentNode cameFrom', PQ.insert fScore neighborNode openSet', closedSet')
    reconstructPath cameFrom targetNode = search targetNode []
      where
        search currentNode path
          | currentNode == sourceNode = (idx currentNode) : path
          | otherwise = search (cameFrom IM.! (idx currentNode)) ((idx currentNode) : path)



-- chat for Vincentry 
vincenty :: (Double, Double) -> (Double, Double) -> Double
vincenty (lat1, lon1) (lat2, lon2) =
  let a = 6378137 -- semi-major axis of the WGS-84 ellipsoid
      b = 6356752.3142 -- semi-minor axis of the WGS-84 ellipsoid
      f = (a - b) / a -- flattening of the WGS-84 ellipsoid
      L = (lon2 - lon1) * pi / 180 -- difference in longitudes
      U1 = atan ((1 - f) * tan (lat1 * pi / 180))
      U2 = atan ((1 - f) * tan (lat2 * pi / 180))
      sinU1 = sin U1
      cosU1 = cos U1
      sinU2 = sin U2
      cosU2 = cos U2
  in iterateUntilClose 1e-12 200 (\sigma ->
    let sinSigma = sqrt ((cosU2 * sin (L))^2 + (cosU1 * sinU2 - sinU1 * cosU2 * cos (L))^2)
        cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cos L
        sigma' = atan2 sinSigma cosSigma
        sinAlpha = cosU1 * cosU2 * sin L / sinSigma
        cosSqAlpha = 1 - sinAlpha^2
        uSq = cosSqAlpha * ((a^2 - b^2) / b^2)
        A = 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)))
        B = uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))
    in (sigma, sigma', sinSigma, cosSigma, sinAlpha, cosSqAlpha, uSq, A, B)) 0
  where
    iterateUntilClose tolerance maxIterations f sigma =
      let (result, next, _, _, _, _, _, _, _) = f sigma
      in if abs (result - next) < tolerance || maxIterations == 0
         then result
        
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
