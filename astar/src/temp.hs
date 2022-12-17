{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read 
import Data.Either
import Text.Read (readMaybe)
import Data.Maybe


foo :: T.Text -> Int
foo text = fst $ fromRight (0, "") eVal
  where eVal = decimal text
