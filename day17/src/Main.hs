{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import System.IO

solve1 :: Int -> Int -> Int
solve1 shift length =
  let spin = iterate (step shift) (0, 1, V.singleton 0 :: Vector Int)
      (pos, _, buf) = spin !! length
  in buf V.! (pos + 1)
  where
    step :: Int -> (Int, Int, Vector Int) -> (Int, Int, Vector Int)
    step shift (!pos, !value, buf) =
      let len = V.length buf
          pos' = (pos + shift) `rem` len
          buf' = V.generate (len + 1)
                 (\i -> if i <= pos'
                        then buf V.! i
                        else if i == pos' + 1
                             then value
                             else buf V.! (i - 1))
      in (pos' + 1, value + 1, buf')

solve2 :: Int -> Int -> Int
solve2 shift length =
  find (0, -1, 1, 0, 1)
  where
    find :: (Int, Int, Int, Int, Int) -> Int
    find (!zeroI, !answer, !len, !i, !newValue)
      | newValue > length = answer
      | otherwise =
        let newI = (i + shift) `rem` len
            zeroI' | newI < zeroI = zeroI + 1
                   | otherwise = zeroI

            answer' | newI == zeroI = newValue
                    | otherwise = answer

        in find (zeroI', answer', (len + 1), (newI + 1), (newValue + 1))

main :: IO ()
main = do
  putStr "shift: "
  hFlush stdout
  shift <- read <$> getLine :: IO Int
  putStr "length: "
  hFlush stdout
  length <- read <$> getLine :: IO Int

  print $ solve1 shift 2017
  print $ solve2 shift length
