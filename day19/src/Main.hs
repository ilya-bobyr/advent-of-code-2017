{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (Left, Right)

import Data.Ix (range)
import Data.List (concatMap, elemIndex)
import Data.Maybe (catMaybes)
import Data.Array (Array, (!))
import qualified Data.Array as A


data Cell = Empty
          | Vertical
          | Horizontal
          | Corner
          | Letter Char
          deriving (Show, Eq)

data Direction = Up
               | Right
               | Down
               | Left
               deriving (Eq)

type Board = (Int, Int, Array (Int, Int) Cell)

parseInput :: [String] -> Board
parseInput lines =
  let rows = length lines
      cols = length $ head lines

      parseCell ' ' = Empty
      parseCell '|' = Vertical
      parseCell '-' = Horizontal
      parseCell '+' = Corner
      parseCell l | l >= 'A' && l <= 'Z' = Letter l
      parseCell c = error $ "Unexpected character: '" ++ show c ++ "'"

      cells = A.listArray ((0, 0), (rows - 1, cols - 1)) $
        concatMap (map parseCell) lines

  in (rows, cols, cells)

solve :: Board -> (Int, String)
solve (rows, cols, cells) =
  let Just startCol = elemIndex Vertical $ take cols $ A.elems cells

      invertDirection :: Direction -> Direction
      invertDirection Up = Down
      invertDirection Right = Left
      invertDirection Down = Up
      invertDirection Left = Right

      directions :: Direction -> (Int, Int) -> [Direction]
      directions prevDir (row, col) =
        let up    = cells ! (row - 1, col    )
            right = cells ! (row    , col + 1)
            down  = cells ! (row + 1, col    )
            left  = cells ! (row    , col - 1)
        in filter (\d -> d /= invertDirection prevDir)
           $ catMaybes [ case up of Vertical -> Just Up
                                    Letter _ -> Just Up
                                    otherwise -> Nothing
                       , case right of Horizontal -> Just Right
                                       Letter _ -> Just Right
                                       otherwise -> Nothing
                       , case down of Vertical -> Just Down
                                      Letter _ -> Just Down
                                      otherwise -> Nothing
                       , case left of Horizontal -> Just Left
                                      Letter _ -> Just Left
                                      otherwise -> Nothing
                       ]

      shift :: Direction -> (Int, Int) -> (Int, Int)
      shift Up    (!row, !col) = (row - 1, col    )
      shift Right (!row, !col) = (row    , col + 1)
      shift Down  (!row, !col) = (row + 1, col    )
      shift Left  (!row, !col) = (row    , col - 1)

      collect :: (Int, Int) -> Direction -> (Int, String) -> (Int, String)
      collect pos dir (!count, !acc) =
        let pos' = shift dir pos
            newCell = cells ! pos'
            acc' = case newCell of
                     Letter l -> l : acc
                     otherwise -> acc
        in if newCell /= Corner
           then collect pos' dir (count + 1, acc')
           else case directions dir pos' of
                  [] -> (count + 1, reverse acc)
                  [nextDir] -> collect pos' nextDir (count + 1, acc')
                  otherwise ->
                    error ("Multiple directions: " ++ show pos')

  in collect (0, startCol) Down (1, "")

main :: IO ()
main = do
  input <- getContents
  let (count, path) = solve $ parseInput $ lines input
  putStrLn path
  print (count - 2)
