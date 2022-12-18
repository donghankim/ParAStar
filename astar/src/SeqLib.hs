{-# LANGUAGE OverloadedStrings #-}

module SeqLib
    ( runner
    ) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.IntMap as M
import qualified Data.List as L

import Data.Text.Read 
import Data.Either
import Data.Maybe (fromJust)

data Node = Node { coord :: (Double, Double),
                   edges :: [(Int, Double)],
                   fn    :: Double
                   } deriving (Show)

aa = Node { coord = (0, 20.2), edges = [(0, 2.1), (2,4.2)], fn = 0} 


runner :: IO ()
runner = do
  fp <- openFile "ColumbiaUniversity.txt" ReadMode
  content <- TIO.hGetContents fp
  let nodeList = map (parseLine) (T.lines content)
      nodeMap = M.fromList nodeList
  
  putStr "Graph Size (n): " *> putStrLn (show $ M.size nodeMap) *> putStrLn ""
  sIdx <- putStr "Enter Start Node Index: " *> hFlush stdout *> getLine
  tIdx <- putStr "Enter Destination Node Index: " *> hFlush stdout *> getLine
  
  let path = findPath nodeMap (read sIdx :: Int) (read tIdx :: Int)
      n1 = fromJust $ M.lookup (read sIdx :: Int) nodeMap
      n2 = fromJust $ M.lookup (read tIdx :: Int) nodeMap
  putStrLn (show $ n1)
  putStrLn (show $ n2)
  putStrLn (show $ path)

-- a* search (nodeMap, source, target)
findPath :: M.IntMap Node -> Int -> Int -> Double
findPath hm sIdx tIdx = vincenty (coord sNode) (coord tNode)
  where
    sNode = fromJust $ M.lookup sIdx hm
    tNode = fromJust $ M.lookup tIdx hm


-- Haversine formula (in Km)
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

-- Vincenty forumla
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
         





-- extracting .txt data
parseLine :: T.Text -> (Int, Node)
parseLine tStr = (nKey, node)
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
    node = Node {coord = nCoord, edges = nEdges, fn = 0}


extractInt :: Either String (Int, T.Text) -> Int
extractInt val
  | isRight val = fst $ fromRight (0, "") val
  | otherwise = error "[.txt data] Node index is corrupted..."

extractDouble :: Either String (Double, T.Text) -> Double
extractDouble val
  | isRight val = fst $ fromRight (0, "") val
  | otherwise = error "[.txt data] Node long/lat is corrupted..."







{- 
loadFile :: Handle -> IO ()
loadFile fh = do
  let nodeMap = M.empty
  eof <- hIsEOF fh
  unless eof $ do
    tStr <- TIO.hGetLine fh
    let (key, node) = parseLine tStr
    let nodeMap = M.insert key node nodeMap
    loadFile fh
  hClose fh
-}


