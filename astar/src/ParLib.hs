module ParLib
    ( helloWorld
    ) where

helloWorld :: IO ()
helloWorld = do
  putStrLn "parallel input"

{-

import Control.Monad.Par (Par, runPar)
import System.IO (Handle, IOMode(ReadMode), openFile, hClose, hSeek, SeekMode(AbsoluteSeek))

-- Function to read a specific number of lines from a handle starting at a given position
readLinesFromHandle :: Handle -> Int -> Int -> Par [String]
readLinesFromHandle handle start count = do
  
  -- Move the handle to the starting position
  liftIO $ hSeek handle AbsoluteSeek (fromIntegral start)
  
  -- Read the specified number of lines from the handle
  liftIO $ replicateM count (hGetLine handle)

-- Function to read a specific number of lines from the file using multiple handles
readLinesFromFile :: FilePath -> Int -> Int -> Int -> Par [[String]]
readLinesFromFile filepath lineCount handleCount linesPerHandle = do
  
  -- Open the file with multiple handles
  handles <- liftIO $ mapM (\_ -> openFile filepath ReadMode) [1..handleCount]
  
  -- Close the handles after reading
  liftIO $ mapM_ hClose handles
  
  -- Read the lines from each handle in parallel
  sequence $ map (\(handle, index) -> readLinesFromHandle handle (index * linesPerHandle) linesPerHandle) (zip handles [0..handleCount-1])

lines <- runPar $ readLinesFromFile "file.txt" lineCount handleCount linesPerHandle
-}
