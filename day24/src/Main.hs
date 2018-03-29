{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>))
import Data.Ord (Down(..), comparing)
import Data.List (concatMap, intercalate, sortBy)
import Data.List.Split (splitOn)


type Port = Int
type Component = (Port, Port)

readComponents :: [String] -> [Component]
readComponents =
  map (\line ->
         let [c1, c2] = map read $ splitOn "/" line
         in (c1, c2))

prependComponent :: Component
                 -> [(Port, [Component])]
                 -> [(Port, [Component])]
prependComponent c = map (\(p, cs') -> (p, c:cs'))

allDeletes :: Port -> [Component] -> [(Port, [Component])]
allDeletes _ [] = []
allDeletes port (c@(p1, p2):cs)
  | port == p1 || port == p2 =
    (if port == p1 then p2 else p1, cs)
    : (prependComponent c $ allDeletes port cs)
  | otherwise =
    prependComponent c $ allDeletes port cs

allBridges :: Port -> [Component] -> [[Port]]
allBridges _ [] = []
allBridges port cs =
  concatMap (\(port', cs') ->
               map (port:) $ ([port'] : allBridges port' cs'))
  $ allDeletes port cs

bridgeScore :: [Port] -> Int
bridgeScore ports =
  2 * sum ports - head ports - last ports

solve1 :: [Component] -> Int
solve1 components =
  maximum $ map bridgeScore $ allBridges 0 components

solve2 :: [Component] -> Int
solve2 components =
  snd
  $ head
  $ sortBy (comparing Down)
  $ map (\ps -> (length ps, bridgeScore ps))
  $ allBridges 0 components

main :: IO ()
main = do
  components <- readComponents <$> lines <$> getContents
  -- putStrLn $ intercalate ", " $ map show components
  -- putStrLn $ intercalate "\n" $ map show
  --   $ allBridges 0 components
  print $ solve1 components
  print $ solve2 components
