module Main where

import Control.Applicative ((<$>))
import Data.Bits ((.&.))
import Data.Foldable (sum)

generator :: Int -> Int -> Int
generator factor v = (v * factor) `rem` 2147483647

solve1 :: (Int, Int) -> Int -> Int
solve1 (genAStart, genBStart) total =
  let genA = tail $ iterate (generator 16807) genAStart
      genALower = map (\v -> v .&. 65535) genA
      genB = tail $ iterate (generator 48271) genBStart
      genBLower = map (\v -> v .&. 65535) genB
      lowerMatch = zipWith (==) genALower genBLower

      countMatching total = sum
                            $ map (\v -> if v then 1 else 0)
                            $ take total lowerMatch
  in countMatching total

solve2 :: (Int, Int) -> Int -> Int
solve2 (genAStart, genBStart) total =
  let genA = filter (\v -> v `rem` 4 == 0)
        $ tail $ iterate (generator 16807) genAStart
      genALower = map (\v -> v .&. 65535) genA
      genB = filter (\v -> v `rem` 8 == 0)
        $ tail $ iterate (generator 48271) genBStart
      genBLower = map (\v -> v .&. 65535) genB
      lowerMatch = zipWith (==) genALower genBLower

      countMatching total = sum
                            $ map (\v -> if v then 1 else 0)
                            $ take total lowerMatch
  in countMatching total

main :: IO ()
main = do
  genAStart <- read <$> getLine :: IO Int
  genBStart <- read <$> getLine :: IO Int

  print $ solve1 (genAStart, genBStart) 40000000
  print $ solve2 (genAStart, genBStart) 5000000
