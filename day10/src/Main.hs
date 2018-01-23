module Main where

import Prelude hiding (round)
import Data.Bits (xor)
import Data.Foldable (foldl', foldlM)
import Data.List (foldl1')
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Control.Monad (liftM)
import Numeric (showHex)


oldPos :: (Int, Int, Int) -> Int -> Int
oldPos (from, len, size) i
  | len == 0 = i
  | from <= (from + len - 1) `rem` size =
    if i < from || (from + len - 1) `rem` size < i
    then i
    else pos i
  | from > (from + len - 1) `rem` size =
    if (from + len - 1) `rem` size < i && i < from
    then i
    else if i >= from
         then pos i
         else pos (i + size)
  | otherwise = error ("oldPos failed for: "
                       ++ show (from, len, size) ++ " " ++ show i)
  where pos i = let i' = (-i) + 2 * from + len - 1
                in (i' + size) `rem` size

  -- | from <= (from + len - 1) `rem` size
  --   && (i < from || (from + len - 1) `rem` size < i) = i
  -- | from > (from + len - 1) `rem` size
  --   && (from <= i && i <= (from + len - 1) `rem` size) = i
  -- | from <= (from + len - 1) `rem` size = pos i
  -- | from > (from + len - 1) `rem` size
  --   && from <= i = pos i
  -- | from > (from + len - 1) `rem` size
  --   && i <= (from + len - 1) `rem` size = pos (i + size)
  -- | otherwise = error ("oldPos failed for: "
  --                      ++ (show (from, len, size)) ++ " " ++ show i)

  -- a * from + b = from + len - 1
  -- a * (from + len - 1) + b = from
  --
  -- b = (1 - a) * from + len - 1
  -- a * from + a * len - a + from - a * from + len - 1 = from
  --
  -- b = (1 - a) * from + len - 1
  -- (a + 1) * len - (a + 1) = 0
  --
  -- (a + 1) * (len - 1) = 0
  -- a = -1
  -- b = 2 * from + len - 1
  --
  -- -i + 2 * from + len - 1

test :: (Int, Int) -> [Int] -> IO ()
test (from, len) expected = do
  let size = length expected
      params = (from, len, size)
      actual = [oldPos params i | i <- [0 .. (size - 1)]]
  if actual == expected
    then putStrLn "OK"
    else putStrLn $ ("Mismatch: " ++ show actual ++ "\n"
                      ++ "  " ++ show params ++ " "
                      ++ show expected)

type RoundState = (Int, Int, V.Vector Int)

round :: [Int] -> RoundState -> RoundState
round lengths input =
  foldl' (\(pos, skip, marks) len ->
             let size = V.length marks
                 getV i = marks ! oldPos (pos, len, size) i
                 marks' = V.generate size (\i -> getV i)
                 pos' = (pos + len + skip) `rem` size
             in (pos', skip + 1, marks'))
         input
         lengths

solve1 :: String -> Int
solve1 input =
  let lengths = (map read . splitOn ",") input :: [Int]
      empty = V.enumFromN 0 256 :: V.Vector Int
      (_, _, transformed) = round lengths (0, 0, empty)
  in (transformed ! 0) * (transformed ! 1)

toHex :: Int -> String
toHex v | v < 16 = "0" ++ showHex v ""
toHex v = showHex v ""

solve2 :: String -> String
solve2 input =
  let lengths = (map fromEnum input) ++ [17, 31, 73, 47, 23]
      empty = V.enumFromN 0 256 :: V.Vector Int
      (_, _, sparse) = iterate (round lengths) (0, 0, empty) !! 64
      dense = map (foldl1' xor) $ chunksOf 16 $ V.toList sparse
  in concat $ map toHex dense

main :: IO ()
main = do
  -- test (0, 3) [2, 1, 0, 3, 4]
  -- test (1, 3) [0, 3, 2, 1, 4]
  -- test (0, 4) [3, 2, 1, 0, 4]
  -- test (1, 4) [0, 4, 3, 2, 1]
  -- test (0, 5) [4, 3, 2, 1, 0]
  -- test (1, 5) [1, 0, 4, 3, 2]
  -- test (2, 5) [3, 2, 1, 0, 4]
  -- test (3, 4) [4, 3, 2, 1, 0]
  -- test (3, 0) [0, 1, 2, 3, 4]

  -- let lengths = [3, 4, 1, 5]
  --     empty = V.enumFromN 0 5 :: V.Vector Int
  -- (_, _, transformed) <-
  --   foldlM (\(pos, skip, marks) len ->
  --             do let size = V.length marks
  --                    getV i = marks ! oldPos (pos, len, size) i
  --                    marks' = V.generate size (\i -> getV i)
  --                    pos' = (pos + len + skip) `rem` size
  --                putStrLn $ show (pos, skip, len) ++ "  => " ++ show marks'
  --                return (pos', skip + 1, marks'))
  --   (0, 0, empty)
  --   lengths
  -- print transformed

  -- lengths <- liftM (map read . splitOn ",") $ getLine :: IO [Int]
  -- let empty = V.enumFromN 0 256 :: V.Vector Int
  --     (_, _, transformed) =
  --       foldl' (\(pos, skip, marks) len ->
  --                  let size = V.length marks
  --                      getV i = marks ! oldPos (pos, len, size) i
  --                      marks' = V.generate size (\i -> getV i)
  --                      pos' = (pos + len + skip) `rem` size
  --                  in (pos', skip + 1, marks'))
  --              (0, 0, empty)
  --              lengths
  -- print transformed
  -- print $ (transformed ! 0) * (transformed ! 1)

  -- putStrLn $ solve2 ""
  -- putStrLn "a2582a3a0e66e6e86e3812dcb672a272"
  -- putStrLn $ solve2 "AoC 2017"
  -- putStrLn "33efeb34ea91902bb2f59c9920caa6cd"
  -- putStrLn $ solve2 "1,2,3"
  -- putStrLn "3efbe78a8d82f29979031a4aa0b16a9d"
  -- putStrLn $ solve2 "1,2,4"
  -- putStrLn "63960835bcdc130f0b66d7ff4f6a5a8e"

  input <- getLine
  print $ solve1 input
  putStrLn $ solve2 input


  -- 28730 - too high
