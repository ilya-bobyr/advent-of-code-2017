module Main where

import qualified Data.Set as Set
import Data.List (sort)

isValid :: (String -> String) -> String -> Bool
isValid tr p = check ws (Set.empty)
  where
    ws = map tr $ words p
    check (w:ws) seen = let seen' = Set.insert w seen
                        in if Set.size seen == Set.size seen'
                           then False
                           else check ws seen'
    check [] _ = True

countValid :: (String -> String) -> [String] -> Int
countValid tr ls = length $ filter (\p -> isValid tr p) ls

main :: IO ()
main = do
  input <- getContents
  print $ countValid id $ lines input
  print $ countValid (\w -> sort w) $ lines input
