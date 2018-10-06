#! /usr/bin/env runhaskell

import System.Process
import Data.List

main :: IO ()
main = do
  fullInfo <- readProcess "amixer" ["sget", "Master"] ""
  putStrLn $ filterVolume fullInfo

filterVolume :: String -> String
filterVolume = f1 . head . (filter (isInfixOf "Right:")) . lines
  where
    f1 []       = []
    f1 ('[':xs) = f2 xs
    f1 (x:xs)   = f1 xs
    f2 []       = []
    f2 (']':ys) = []
    f2 (y:ys)   = y:(f2 ys)
