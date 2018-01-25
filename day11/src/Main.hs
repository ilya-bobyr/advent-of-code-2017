module Main where

import Data.List.Split (splitOn)
import Data.List (foldl')

type Coord = (Int, Int) -- (x, y)
type CoordDelta = (Int, Int) -- (dx, dy)

parse :: String -> [CoordDelta]
parse input =
  map p $ splitOn "," input
  where p "n"  = ( 0,  1)
        p "ne" = ( 1,  0)
        p "se" = ( 1, -1)
        p "s"  = ( 0, -1)
        p "sw" = (-1,  0)
        p "nw" = (-1,  1)

distance :: Coord -> Int
distance (x, y) = maximum $ map abs [x, y, (x + y)]

foldStep :: (Coord, Int) -> CoordDelta -> (Coord, Int)
foldStep ((px, py), maxDist) (dx, dy) =
  let pos' = (px + dx, py + dy)
      newDist = distance pos'
  in (pos', max maxDist newDist)

main :: IO ()
main = do
  input <- getLine
  let deltas = parse input
      (final, maxDist) = foldl' foldStep ((0, 0), 0) deltas
  print $ distance final
  print maxDist
