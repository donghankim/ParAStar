{-# LANGUAGE OverloadedStrings #-}

module SeqLib (seqSearch) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Data.PQueue.Min as P

import Data.Text.Read
import Data.Either
import Data.Maybe (fromJust, isJust)

-- debug use ii
{-
stack repl
:set -fprint-evld-with-show
nn = Node {idx = (-1), coord = (1.2, 2.1), edges = []}

aa = M.fromList [(1,2.1), (2, 10)] :: M.IntMap Double
foo idx aa = M.update (\x -> if x == 10.0 then Just 100 else Nothing) idx aa

bb = P.fromList [(20,1), (0,5)]
minVal = P.getMin bb
-}


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
  putStrLn "*** Sequential A-Star Path Finder ***"
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
      path = astar sIdx tIdx nodeMap openList closedList cameFrom

  case path of
       Nothing -> return ([])
       _       -> return (fromJust path)


-- | Find shortest path using A* search
-- args: sIdx, tIdx, nodeMap, openList, closedList, cameFrom
-- sIdx => start node idx
-- tIdx => target node idx
-- nodeMap => IntMap <idx, Node>
-- openList => MinPQueue <fn, idx>
-- closedSet => IntSet <idx>
-- cameFrom => IntMap <fromIdx, toIdx>
-- return: path :: Maybe [Int]
astar :: Int -> Int -> M.IntMap Node -> P.MinQueue (Double, Int) -> S.IntSet -> M.IntMap Int ->  Maybe [Int]
astar sIdx tIdx nodeMap openList closedSet cameFrom
  | P.null openList         = Nothing
  | cIdx == tIdx            = Just $ reconstructPath sIdx tIdx cameFrom
  | S.member cIdx closedSet = astar sIdx tIdx nodeMap openList' closedSet cameFrom
  | otherwise               = astar sIdx tIdx nodeMap openList'' closedSet' cameFrom''
  where
    cIdx = snd (P.findMin openList)
    openList' = P.deleteMin openList
    closedSet' = S.insert cIdx closedSet
    cNode = nodeMap M.! cIdx

    -- calculate f(n) = g(n) + h(n)
    adjNodes = filter ((\adjIdx -> S.notMember adjIdx closedSet').fst) (edges cNode)
    cameFrom' = updateFrom cameFrom [(adjIdx, cIdx) | (adjIdx, _) <- adjNodes]
    fcost = [(adjIdx, calcFn adjIdx sIdx tIdx nodeMap cameFrom') | (adjIdx, _) <- adjNodes]

    -- update shared resource
    combinedOpen = updateOpen openList' fcost
    updatedNodes = map snd (P.elemsU $ fst combinedOpen)
    cameFrom'' = M.mapWithKey (\idx old -> if idx `elem` updatedNodes then cIdx else old) cameFrom'
    openList'' = P.union (fst combinedOpen) (snd combinedOpen)


-- | Update openList
updateOpen :: P.MinQueue (Double, Int) -> [(Int, Double)] -> (P.MinQueue (Double, Int), P.MinQueue (Double, Int))
updateOpen openList fcost =
  let
    currentNodes = map snd (P.elemsU openList)
    newNodes = [(fn, idx) | (idx, fn) <- fcost, idx `notElem` currentNodes]
    newOpen = P.fromList newNodes

    temp = P.partition (\(fn, idx) -> let fn' = lookup idx fcost in (isJust fn' && fromJust fn' < fn)) openList
    sameOpen = snd temp
    updatedOpen = P.map (\(_, idx) -> (fromJust $ lookup idx fcost, idx)) (fst temp)
  in if P.null openList then (P.union newOpen updatedOpen, sameOpen) else (P.union newOpen updatedOpen, sameOpen)


-- | Update cameFrom
updateFrom :: M.IntMap Int -> [(Int, Int)] -> M.IntMap Int
updateFrom cameFrom adjNodes = foldl f cameFrom adjNodes
  where
    f cameFrom (from,to) = if M.member from cameFrom then cameFrom else M.insert from to cameFrom


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


-- | Calculate hn 
calcHn :: Node -> Node -> Double
calcHn cNode tNode = vincenty (coord cNode) (coord tNode)


-- | Reconstruct the shortest path (update later)
reconstructPath :: Int -> Int -> M.IntMap Int -> [Int]
reconstructPath sIdx tIdx cameFrom
  | sIdx == tIdx = [sIdx]
  | otherwise = reconstructPath sIdx (cameFrom M.! tIdx) cameFrom ++ [tIdx]


-- | Vincenty forumla (WGS-84 standard, in meters)
-- accurate to upto 0.066mm, but expensive
vincenty :: (Double, Double) -> (Double, Double) -> Double
vincenty n1 n2 =
  let lon1 = fst n1
      lat1 = snd n1
      lon2 = fst n2
      lat2 = snd n2
      a = 6378137 :: Double
      f = 1/298.257223563 :: Double
      b = (1 - f) * a :: Double
      capL = (lon2 - lon1) * pi / 180 :: Double
      u1 = atan ((1 - f) * tan (lat1 * pi / 180)) :: Double
      u2 = atan ((1 - f) * tan (lat2 * pi / 180)) :: Double
      sinU1 = sin u1 :: Double
      cosU1 = cos u1 :: Double
      sinU2 = sin u2 :: Double
      cosU2 = cos u2 :: Double

  in iterateUntilClose 1e-12 10000 (\lambda ->
    let
        sinSigma = sqrt ((cosU2 * sin (lambda))**2 + (cosU1 * sinU2 - sinU1 * cosU2 * cos (lambda))**2) :: Double
        cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cos lambda :: Double
        sigma = atan (sinSigma/cosSigma) :: Double
        sinAlpha = cosU1 * cosU2 * (sin lambda) / sinSigma :: Double
        cosSqAlpha = 1 - sinAlpha**2 :: Double
        cos2sig = cosSigma - (2*sinU1*sinU2/cosSqAlpha) :: Double
        capC = (f/16)*cosSqAlpha*(4 + f*(4 - 3*cosSqAlpha)) :: Double
        lambda' = capL + (1 - capC) * f * sinAlpha * (sigma + capC * sinSigma * (cos2sig + capC * cosSigma * (-1 + 2 * cos2sig**2))) :: Double
    in (lambda, lambda', sigma, cosSqAlpha, cosSigma, cos2sig, sinSigma, a, b)) capL
  where
    iterateUntilClose tolerance maxIterations f lambda =
      let (prevLambda, newLambda, sig, csa, cs, c2s, ss, defA, defB) = f lambda
      in if abs (prevLambda - newLambda) < tolerance || maxIterations == 0
         then vincentyDistance ((csa**2)*(defA**2 - defB**2)/defB**2) ss c2s cs defB sig
         else iterateUntilClose tolerance (maxIterations - 1) f newLambda


-- Vincenty helper (calculate distance)
vincentyDistance :: Double -> Double -> Double -> Double -> Double -> Double -> Double
vincentyDistance uSq sinSig cosTwoSig cosSig b sig' = b*capA*(sig' - deltaSig)
  where
    capA = 1 + (uSq/16384) * (4096 + uSq*(-768 + uSq * (320 - 175*uSq)))
    capB = (uSq/1024)*(256 + uSq*(-128 + uSq*(74 - 47*uSq)))
    deltaSig = capB*sinSig*(cosTwoSig +(1/4)*capB*(cosSig*(-1 +2*cosTwoSig**2) - (1/6)*capB*cosTwoSig*(-3 + 4*sinSig**2)*(-3 + 4*cosTwoSig**2)))


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
                        let id_ = extractInt $ decimal $ idVal,
                        let wt = extractDouble $ rational $ wVal]

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














