{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>))
import Data.List (take, drop, (!!))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

dance :: [String] -> String -> String

dance ![] !line = line

dance !(('s' : numStr) : commands) !line =
  let num = read numStr
      prefixLen = length line - num
      line' = drop prefixLen line ++ take prefixLen line
  in dance commands line'

dance !(('x' : args) : commands) !line =
  let [i, j] = map read $ splitOn "/" args
      [i', j'] = if i < j then [i, j] else [j, i]

      swapIndices (idx, c)
        | idx == i' = line !! j'
        | idx == j' = line !! i'
        | otherwise = c
      line' = [swapIndices pos | pos <- zip [0..] line]
  in dance commands line'

dance !(('p' : args) : commands) !line =
  let [name1:[], name2:[]] = splitOn "/" args
      swapNames c
        | c == name1 = name2
        | c == name2 = name1
        | otherwise = c
      line' = [swapNames c | c <- line]
  in dance commands line'


findLoop :: [String] -> String -> (Int, Int)
findLoop commands line =
  find line 1 (Map.singleton line 0)
  where find line index !seen =
          let line' = dance commands line
              seen' = Map.insert line' index seen
          in case Map.lookup line' seen of
               Just prevIndex -> (prevIndex, index)
               Nothing -> find line' (index + 1) seen'


main :: IO ()
main = do
  -- print $ solve1 "abcde" ["s1", "x3/4", "pe/b"]

  commands <- splitOn "," <$> getLine
  print $ dance commands ['a'..'p']

  let (from, to) = findLoop commands ['a'..'p']
      targetIndex = (1000000000 - from) `rem` (to - from)
      dances = iterate (dance commands) ['a'..'p']

  print $ dances !! targetIndex
