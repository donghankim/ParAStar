{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read 
import Data.Either
import Text.Read (readMaybe)
import Data.Maybe



-- chat 
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
        
