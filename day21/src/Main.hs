{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Data.Bits ((.|.), shift, bit)
import Data.Foldable (foldl')
import Data.List (intercalate)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Traversable (traverse)
import Control.Applicative ((<$>), (<*))
import Control.Monad (unless)
import Text.Parsec (parse)
import Text.Parsec.Prim (many, parserFail)
import Text.Parsec.Char (char, string, newline, oneOf)
import Text.Parsec.Combinator (many1, option, eof, sepBy)
import Text.Parsec.String (Parser)


type GridImpl = UArray (Int, Int) Bool
newtype Grid = Grid GridImpl

instance Show Grid where
  show (Grid grid) =
    let (_, (maxRow, maxCol)) = UA.bounds grid

        showRow rowI =
          map (\colI -> if grid UA.! (rowI, colI)
                     then '#' else '.') [0 .. maxCol]

    in intercalate "/"
       $ map (\rowI -> showRow rowI) [0 .. maxRow]

data Rule = Rule {
    ruleInput :: Grid,
    ruleOutput :: Grid
  }

instance Show Rule where
  show (Rule input output) =
    show input ++ " => " ++ show output


inputParser :: Parser [Rule]
inputParser = many ruleParser <* eof

hspaces :: Parser String
hspaces = many $ oneOf " \t"

ruleParser :: Parser Rule
ruleParser = do
  let row = many1 $ oneOf ".#"
      readRow str =
        map (\c -> c == '#') str

      readGrid rows = map readRow rows

  input <- readGrid <$> (row `sepBy` char '/')

  let inputSize = length input
      inputIsSquare = and $ map (\r -> length r == inputSize) input
  unless inputIsSquare $ parserFail "Input is not a square"

  hspaces
  string "=>"
  hspaces
  output <- readGrid <$> (row `sepBy` char '/')

  let outputSize = length output
      outputIsSquare = and $ map (\r -> length r == outputSize) output
  unless outputIsSquare $ parserFail "Output is not a square"

  unless (inputSize + 1 == outputSize)
    $ parserFail "Output size is not one larger than input size"

  newline

  let squareBounds dim = ((0, 0), (dim - 1, dim - 1))
      toGrid (size, list) =
        Grid $ UA.listArray (squareBounds size) $ concat list

  return $ Rule (toGrid (inputSize, input)) (toGrid (outputSize, output))

newtype Board = Board Grid

instance Show Board where
  show (Board board) = show board

boardSize :: Board -> Int
boardSize (Board (Grid grid)) =
  let (_, (maxRow, _)) = UA.bounds grid
  in maxRow + 1

gridVariants :: Grid -> [Grid]
gridVariants (Grid grid) =
  let bounds = UA.bounds grid
      (_, (maxRow, maxCol)) = bounds
      xform f = Grid $ UA.ixmap bounds f grid
  in [ Grid grid
     , xform $ \(row, col) -> (row         , maxCol - col)
     , xform $ \(row, col) -> (maxRow - row,          col)
     , xform $ \(row, col) -> (maxRow - row, maxCol - col)
     , xform $ \(row, col) -> (         col,          row)
     , xform $ \(row, col) -> (maxRow - col,          row)
     , xform $ \(row, col) -> (         col, maxCol - row)
     , xform $ \(row, col) -> (maxRow - col, maxCol - row)
     ]

gridHash :: Grid -> Int
gridHash (Grid grid) =
  let (_, (maxRow, _)) = UA.bounds grid
  in (maxRow `shift` 10)
     .|. (foldl' (\h v -> (h `shift` 1) .|. v) 0
          $ map (\v -> if v then 1 else 0)
          $ UA.elems grid)

extractGrid :: Board -> Int -> (Int, Int) -> Grid
extractGrid (Board (Grid grid)) patchSize (row, col) =
  Grid $ UA.ixmap ((0, 0), (patchSize - 1, patchSize - 1))
                  (\(r, c) -> (row + r, col + c))
                  grid

gridAsListShifted :: (Int, Int) -> Grid -> [((Int, Int), Bool)]
gridAsListShifted (rowShift, colShift) (Grid grid) =
  map (\((row, col), value) ->
          ((row + rowShift, col + colShift), value))
  $ UA.assocs grid

rulesDictionary :: [Rule] -> IntMap Grid
rulesDictionary rules =
  let go ((Rule input output) : rs) dict =
        let dict' = foldl' (\d k -> IntMap.insert k output d) dict
                    $ map gridHash $ gridVariants input
        in go rs dict'
      go [] dict = dict

  in go rules IntMap.empty

transform :: [Rule] -> Board -> Int -> Board
transform rules initialBoard count =
  let dict = rulesDictionary rules

      gridXfrm ::
        Board -> Int -> (Int, Int) -> (Int, Int) -> [((Int, Int), Bool)]
      gridXfrm board patchSize coord coord' =
        let hash = gridHash $ extractGrid board patchSize coord
            Just grid' = IntMap.lookup hash dict
        in gridAsListShifted coord' grid'

      step :: Board -> Board
      step board@(Board (Grid grid))
        | size `rem` 2 == 0 =
          let newCoord (row, col) = ( (row + 1) * 3 `quot` 2 - 1
                                    , (col + 1) * 3 `quot` 2 - 1)
          in Board $ Grid $ UA.array ((0, 0), newCoord (maxRow, maxCol))
             $ concatMap (\c -> gridXfrm board 2 c (newCoord c))
             $ [(row, col) | row <- [0, 2 .. (maxRow - 1)]
                           , col <- [0, 2 .. (maxCol - 1)]]

        | otherwise =
          let newCoord (row, col) = ( (row + 1) * 4 `quot` 3 - 1
                                    , (col + 1) * 4 `quot` 3 - 1)
          in Board $ Grid $ UA.array ((0, 0), newCoord (maxRow, maxCol))
             $ concatMap (\c -> gridXfrm board 3 c (newCoord c))
             $ [(row, col) | row <- [0, 3 .. (maxRow - 1)]
                           , col <- [0, 3 .. (maxCol - 1)]]

        where
          (_, (maxRow, maxCol)) = UA.bounds grid
          size = boardSize board

  in iterate step initialBoard !! count


main :: IO ()
main = do
  content <- getContents
  case parse inputParser "<input>" content of
    Left error -> print error
    Right rules -> do
      {-
         .#.
         ..#
         ###
      -}
      let initialBoard = Board $ Grid
            $ UA.listArray ((0, 0), (2, 2)) [
              False, True, False
            , False, False, True
            , True, True, True
            ]
          onPixelCount iters =
            case transform rules initialBoard iters of
              Board (Grid grid) ->
                foldl' (\count v -> if v
                                    then count + 1
                                    else count) 0
                $ UA.elems grid

      print $ onPixelCount 5
      print $ onPixelCount 18

      -- putStrLn $ intercalate "\n" $ map show rules
