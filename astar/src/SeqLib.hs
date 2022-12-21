{-# LANGUAGE OverloadedStrings #-}

module SeqLib (seqSearch) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Data.PQueue.Prio.Min as P
-- import qualified Data.PQueue.Min as PP

import Data.Text.Read 
import Data.Either
import Data.Maybe (fromJust, isNothing)



-- | Node datatype
-- idx: Node index (used as key in nodeMap)
-- coord: lat,lon coordinates
-- edges: list of (idx,gn) tuples
data Node = Node { idx   :: Int,
                   coord :: (Double, Double),
                   edges :: [(Int, Double)]
                   } deriving (Show)



-- | Initiate sequential A* search on
-- selected OpenStreetMap graph
seqSearch :: IO [Int]
seqSearch = do
  fp <- openFile "ColumbiaUniversity.txt" ReadMode
  content <- TIO.hGetContents fp
  start <- putStr "Enter Start Node Index: " *> hFlush stdout *> getLine
  target <- putStr "Enter Destination Node Index: " *> hFlush stdout *> getLine
  
  let nodeMap = M.fromList $ map (parseLine) (T.lines content)
      sIdx = read start :: Int
      tIdx = read target :: Int
      openList = P.singleton 0 sIdx :: P.MinPQueue Double Int
      closedList = S.empty
      cameFrom = M.empty :: M.IntMap Int
      path = astar sIdx tIdx nodeMap openList closedList cameFrom
  
  case path of 
       Nothing -> return([])
       isJust  -> return(fromJust path)



-- | Find shortest path using A* search
-- args: sIdx, tIdx, nodeMap, openList, closedList, cameFrom
-- sIdx => start node idx
-- tIdx => target node idx
-- nodeMap => IntMap <idx, Node>
-- openList => MinPQueue <fn, idx>
-- closedSet => IntSet <idx> 
-- cameFrom => IntMap <fromIdx, toIdx>
-- return: path :: Maybe [Int]
astar :: Int -> Int -> M.IntMap Node -> P.MinPQueue Double Int -> S.IntSet -> M.IntMap Int -> Maybe [Int]
astar sIdx tIdx nodeMap openList closedSet cameFrom
  | cIdx == tIdx = Just $ reconstructPath sIdx tIdx cameFrom
  | P.null openList = Nothing
  | S.member cIdx closedSet = astar sIdx tIdx nodeMap openList' closedSet' cameFrom
  | otherwise = astar sIdx tIdx nodeMap openList'' closedSet' cameFrom
  where
    cIdx = snd (fromJust $ P.getMin openList)
    cNode = nodeMap M.! cIdx
    openList' = P.deleteMin openList
    adjNodes = (filter ((\idx -> S.notMember idx closedSet').fst) (edges cNode))
    fcost = [(calcFn idx sIdx tIdx nodeMap cameFrom, idx) | (idx, _) <- adjNodes]
    
    openList'' = P.union openList' $ P.fromList fcost
    closedSet' = S.insert cIdx closedSet

    
{-
-- | Update openList and cameFrom
updateShared :: P.MinPQueue -> [(Double, Int)] -> M.IntMap Int -> (P.MinPQueue Double Int -> M.IntMap Int)
updateShared oldOpen fcost oldFrom = newOpen newFrom
  where
-}

-- | Calculate fn
calcFn :: Int -> Int -> Int -> M.IntMap Node -> M.IntMap Int -> Double
calcFn cIdx sIdx tIdx nodeMap cameFrom = 
  let 
    gn = calcGn cIdx sIdx nodeMap cameFrom 0.0
    hn = calcHn (nodeMap M.! cIdx) (nodeMap M.! tIdx)
  in gn + hn
    

-- | Calculate gn
calcGn :: Int -> Int -> M.IntMap Node -> M.IntMap Int -> Double -> Double
calcGn cIdx sIdx nodeMap cameFrom gn
  | cIdx == sIdx = gn
  | otherwise = calcGn fIdx sIdx nodeMap cameFrom gn'
  where
    fIdx = cameFrom M.! cIdx
    gn' = gn + (snd . head $ filter ((\idx -> idx == cIdx).fst) (edges $ nodeMap M.! fIdx))
 

-- | Calculate hn (replace with Vincenty later)
calcHn :: Node -> Node -> Double
calcHn cNode tNode = 0.0


-- | Reconstruct the shortest path (update later)
reconstructPath :: Int -> Int -> M.IntMap Int -> [Int]
reconstructPath sIdx tIdx cameFrom = [202,90,31,86,82,72]























-- | Vincenty forumla (WGS-84 standard)
-- accurate to upto 0.066 meters, but expensive
vincenty :: (Double, Double) -> (Double, Double) -> Double
vincenty n1 n2 =
  let lon1 = fst n1
      lat1 = snd n1
      lon2 = fst n2
      lat2 = snd n2
      a = 6378137 -- semi-major axis of the WGS-84 ellipsoid
      b = 6356752.3142 -- semi-minor axis of the WGS-84 ellipsoid
      f = (a - b) / a -- flattening of the WGS-84 ellipsoid
      l = (lon2 - lon1) * pi / 180 -- difference in longitudes
      u1 = atan ((1 - f) * tan (lat1 * pi / 180))
      u2 = atan ((1 - f) * tan (lat2 * pi / 180))
      sinU1 = sin u1
      cosU1 = cos u1
      sinU2 = sin u2
      cosU2 = cos u2
  in iterateUntilClose 1e-12 5000 (\sigma ->
    let sinSigma = sqrt ((cosU2 * sin (l))^2 + (cosU1 * sinU2 - sinU1 * cosU2 * cos (l))^2)
        cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cos l
        sigma' = atan2 sinSigma cosSigma
        sinAlpha = cosU1 * cosU2 * sin l / sinSigma
        cosSqAlpha = 1 - sinAlpha^2
        uSq = cosSqAlpha * ((a^2 - b^2) / b^2)
        aA = 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)))
        bB = uSq / 1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))
    in (sigma, sigma', sinSigma, cosSigma, sinAlpha, cosSqAlpha, uSq, aA, bB)) 0
  where
    iterateUntilClose tolerance maxIterations f sigma =
      let (result, next, _, _, _, _, _, _, _) = f sigma
      in if abs (result - next) < tolerance || maxIterations == 0
         then result
         else iterateUntilClose tolerance (maxIterations - 1) f next
         

-- | Haversine formula (in Km)
-- not very accurate when lat/long coordinates are close
haversine :: (Double, Double) -> (Double, Double) -> Double
haversine n1 n2 = 2 * 6371 * asin (sqrt (sin (dLat/2)^2 + cos lat1' * cos lat2' * sin (dLon/2)^2))
  where
    lon1 = fst n1
    lat1 = snd n1
    lon2 = fst n2
    lat2 = snd n2
    dLat = (lat2 - lat1) * (pi/180)
    dLon = (lon2 - lon1) * (pi/180)
    lat1' = lat1 * (pi/180)
    lat2' = lat2 * (pi/180)


-- | Parse each line in graph.txt file
-- return the node index and newly created Node datatype 
parseLine :: T.Text -> (Int, Node)
parseLine tStr = (nKey, node)
  where
    [idx, info] = T.splitOn ":" tStr
    [coord, edges] = T.splitOn "*" info
    [yt, xt] = T.splitOn "&" coord 
    edgeList = T.splitOn "," edges
    nEdges = [(id_, wt) | dt <- edgeList, 
                        let [idVal, wVal] = T.splitOn ";" dt,
                        let id_ = extractInt $ decimal idVal,
                        let wt = extractDouble $ rational wVal]
    
    nKey = extractInt $ decimal idx
    nCoord = (extractDouble $ rational yt, extractDouble $ rational xt)
    node = Node {idx = nKey, coord = nCoord, edges = nEdges}


extractInt :: Either String (Int, T.Text) -> Int
extractInt val
  | isRight val = fst $ fromRight (0, "") val
  | otherwise = error "[.txt data] Node index is corrupted..."

extractDouble :: Either String (Double, T.Text) -> Double
extractDouble val
  | isRight val = fst $ fromRight (0, "") val
  | otherwise = error "[.txt data] Node long/lat is corrupted..."


-- debug use
nn = Node {idx = (-1), coord = (1.2, 2.1), edges = []}

