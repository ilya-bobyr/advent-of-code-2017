{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits (bit, setBit, testBit)
import qualified Data.BitVector as BV
import Data.BitVector (BV)
import Data.Foldable (foldr')
import Data.List (null, partition)
import Text.Parsec (parse)
import Text.Parsec.Prim (many, try)
import Text.Parsec.Char (char, oneOf, digit, string, newline)
import Text.Parsec.Combinator (many1, sepBy, eof)
import Text.Parsec.String (Parser)

inputParser :: Parser [(Int, [Int])]
inputParser = do
  result <- many lineParser
  eof
  return result

hspaces :: Parser String
hspaces = many $ oneOf " \t"

lineParser :: Parser (Int, [Int])
lineParser = do fromStr <- many1 digit
                hspaces >> string "<->" >> hspaces
                to <- do many1 digit `sepBy` (hspaces >> char ',' >> hspaces)
                newline
                return $ (read fromStr, map read to)

type Conns = (Int, [Int])

extractGroup :: [Conns] -> (BV, [Conns])
extractGroup connections =
  let size = length connections

      markAll :: BV -> [Int] -> BV
      markAll v [] = v
      markAll v (p:ps) = markAll (v `setBit` p) ps

      nextWave :: BV -> [Conns] -> ([Conns], [Conns])
      nextWave inSet queue =
        partition (\(i, _) -> inSet `testBit` i) queue

      loop :: BV -> [Conns] -> (BV, [Conns])
      loop inSet queue =
        let (front, skipped) = nextWave inSet queue
        in if null front
           then (inSet, skipped)
           else let inSet' =
                      foldr' (\(_, to) v -> markAll v to) inSet front
                in loop inSet' skipped

      firstIndex = fst $ head connections

  in loop (bit firstIndex) connections

solve1 :: [Conns] -> Int
solve1 connections =
  let (group0, _) = extractGroup connections
  in BV.foldr (\v sum -> if v then sum + 1 else sum) 0 group0

solve2 :: [Conns] -> Int
solve2 connections =
  let count i [] = i
      count !i connections =
        let (_, connections') = extractGroup connections
        in count (i+1) connections'
  in count 0 connections

main :: IO ()
main = do
  content <- getContents
  case parse inputParser "<input>" content of
    Left error -> print error
    Right connections -> do
      print $ solve1 connections
      print $ solve2 connections
