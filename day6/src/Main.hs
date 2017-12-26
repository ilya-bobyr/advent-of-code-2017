{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Array (Array, array, listArray, bounds, (!), assocs,
                   elems)
import Data.Foldable (foldl')
import Data.Functor (fmap)
import qualified Data.Map.Strict as M (empty, insert, lookup)

type Banks = Array Int Int

balance :: Banks -> Banks
balance banks =
  let values = assocs banks
      pickLargest (i1, v1) (i2, v2) =
        if v1 >= v2
        then (i1, v1)
        else (i2, v2)
      (from, value) = foldl' pickLargest (-1, 0) values
      count = (snd $ bounds banks) - (fst $ bounds banks) + 1
      distribuite (i, v) =
        (i, (value `quot` count)
          + (if i == from then 0 else v)
          + (if (i - (from + 1) + count) `rem` count < value `rem` count
             then 1 else 0))
  in array (bounds banks) $ map distribuite values

findLoop :: [Int] -> (Int, Int)
findLoop vs =
  let banks = listArray (0, length vs - 1) vs
  in go M.empty banks 0
  where go seen banks !c =
          case M.lookup banks seen of
            Just i -> (c - i, c)
            Nothing -> go (M.insert banks c seen) (balance banks) (c + 1)

main :: IO ()
main = do
  banks <- fmap (map read . words) $ getLine :: IO [Int]
  let (loopSize, steps) = findLoop banks
  print steps
  print loopSize
