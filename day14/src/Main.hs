module Main where

import Prelude hiding (round)
import Control.Monad.State.Strict (State, runState, get, put)
import Data.Bits (Bits, xor, popCount, testBit, shift, (.|.))
import Data.Foldable (foldl')
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

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

hash :: String -> [Int]
hash input =
  let lengths = (map fromEnum input) ++ [17, 31, 73, 47, 23]
      empty = V.enumFromN 0 256 :: V.Vector Int
      (_, _, sparse) = iterate (round lengths) (0, 0, empty) !! 64
      dense = map (foldl1' xor) $ chunksOf 16 $ V.toList sparse
  in dense

grid :: String -> [Integer]
grid key = map (\i -> joinInts $ hash $ key ++ "-" ++ show i) [0..127]
  where
    joinInts = foldl' (\acc v -> acc `shift` 8 .|. toInteger v) 0

countBits :: (Num a, Bits a) => [a] -> Int
countBits hash = sum $ map popCount hash

solve1 :: String -> Int
solve1 key = countBits $ grid key


type Solution2State = (Int, IntMap IntSet)

processLine :: [Bool] -- grid line bits
            -> Int    -- left group
            -> [Int]  -- previous line groups
            -> [Int]  -- reversed line groups accumulator
            -> State Solution2State [Int]

processLine [] _ _ acc = return $ reverse acc

processLine (False:sq) _ (_:gs) acc =
  processLine sq 0 gs (0:acc)

processLine (True:sq) 0 (0:gs) acc = do
  (nextGroup, groups) <- get
  let nextGroupSet = IntSet.singleton nextGroup
  put (nextGroup + 1, IntMap.insert nextGroup nextGroupSet groups)
  processLine sq nextGroup gs (nextGroup:acc)

processLine (True:sq) 0 (topGroup:gs) acc =
  processLine sq topGroup gs (topGroup:acc)

processLine (True:sq) leftGroup (0:gs) acc =
  processLine sq leftGroup gs (leftGroup:acc)

processLine (True:sq) leftGroup (topGroup:gs) acc | leftGroup == topGroup =
  processLine sq leftGroup gs (leftGroup:acc)

processLine (True:sq) leftGroup (topGroup:gs) acc = do
  (nextGroup, groups) <- get
  let Just leftGroupSet = IntMap.lookup leftGroup groups
      Just topGroupSet = IntMap.lookup topGroup groups
      mergedGroupSet = IntSet.union leftGroupSet topGroupSet
      -- groups' = IntMap.insert topGroup mergedGroupSet $
      --           IntMap.insert leftGroup mergedGroupSet groups
      groups' = foldl' (\groups i -> IntMap.insert i mergedGroupSet groups)
                       groups (IntSet.toList mergedGroupSet)
      minGroup = min topGroup leftGroup
  put (nextGroup, groups')
  processLine sq minGroup gs (minGroup:acc)

findGroups :: Int -> [Integer] -> IntMap IntSet
findGroups width grid =
  let (_, (_, groups)) =
        foldl' (\(prevLine, state) diskLine ->
                  let bits = map (testBit diskLine) [width, (width - 1)..0]
                  in runState (processLine bits 0 prevLine []) state
               ) (replicate (width+1) 0, (1, IntMap.empty)) grid
  in groups

processGrid :: Int -> [Integer] -> Int
processGrid width grid =
  Map.size
  $ Map.fromListWith (+)
  $ map (\v -> (v, 1))
  $ IntMap.elems
  $ findGroups width grid

solve2 :: String -> Int
solve2 key = processGrid 128 $ grid key

main :: IO ()
main = do
  key <- getLine
  print $ solve1 key
  print $ solve2 key

  -- let bits = map (testBit (0 :: Integer)) [4, 3 .. 0]
  --     prevLine = replicate 4 0
  --     emptyState = (0, IntMap.empty)
  -- print $ runState (processLine bits 0 (0 : prevLine) []) emptyState

  -- print $ findGroups 4 [1, 3, 0, 0]
  -- print $ runState (processLine (map (testBit (1 :: Integer)) [4, 3..0])
  --                               0 (replicate 5 0) [])
  --                  (1, IntMap.empty)
