{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Data.Array.ST (STUArray)
import qualified Data.Array.ST as STUA
import Control.Applicative ((<$>))
import Control.Monad (unless, foldM)
import Control.Monad.ST (ST, runST)
import Data.List (intercalate)
import Data.Word (Word8)
import Text.Parsec (parse)
import Text.Parsec.Prim (many, parserFail)
import Text.Parsec.Char (newline, oneOf)
import Text.Parsec.Combinator (many1, eof, sepEndBy)
import Text.Parsec.String (Parser)


data NodeState = Clean
               | Weakened
               | Infected
               | Flagged
               deriving (Eq, Enum)

newtype Board = Board (UArray (Int, Int) Word8)

instance Show Board where
  show (Board grid) =
    let (_, (maxRow, maxCol)) = UA.bounds grid

        showRow rowI =
          map (\colI ->
                 case toEnum . fromEnum $ grid UA.! (rowI, colI) of
                   Clean -> '.'
                   Weakened -> 'W'
                   Infected -> '#'
                   Flagged -> 'F') [0 .. maxCol]

    in intercalate "\n"
       $ map (\rowI -> showRow rowI) [0 .. maxRow]


inputParser :: Int -> Parser Board
inputParser padding = do
  let row = many1 $ oneOf ".#"
      readRow str =
        map (\c -> if c == '#'
                   then toEnum . fromEnum $ Infected
                   else toEnum . fromEnum $ Clean) str

      readGrid = map readRow

  input <- readGrid <$> (row `sepEndBy` newline)

  let rowCount = length input
      inputIsSquare = and $ map (\r -> length r == rowCount) input
  unless inputIsSquare $ parserFail "Input is not a square"

  eof

  let empty = STUA.runSTUArray $
              STUA.newArray ((0, 0), ( 2 * padding + rowCount - 1
                                     , 2 * padding + rowCount - 1))
              $ toEnum . fromEnum $ Clean

  return $ Board $ empty UA.// (
    concat
    $ zipWith (\rowI row ->
                 zipWith (\colI v ->
                            ((rowI, colI), v)) [padding..] row
              ) [padding..] input
    )

boardCenter :: Board -> (Int, Int)
boardCenter (Board grid) =
  let (_, (maxRow, maxCol)) = UA.bounds grid
  in ((maxRow + 1) `quot` 2, (maxCol + 1) `quot` 2)

data Direction = DUp
               | DRight
               | DDown
               | DLeft

type CarrierInfo = (Direction, Int, Int)

carrierPos :: CarrierInfo -> (Int, Int)
carrierPos (_, row, col) = (row, col)

turnRight :: CarrierInfo -> CarrierInfo
turnRight (DUp,    row, col) = (DRight, row, col)
turnRight (DRight, row, col) = (DDown,  row, col)
turnRight (DDown,  row, col) = (DLeft,  row, col)
turnRight (DLeft,  row, col) = (DUp,    row, col)

turnLeft :: CarrierInfo -> CarrierInfo
turnLeft (DUp,    row, col) = (DLeft,  row, col)
turnLeft (DRight, row, col) = (DUp,    row, col)
turnLeft (DDown,  row, col) = (DRight, row, col)
turnLeft (DLeft,  row, col) = (DDown,  row, col)

turnReverse :: CarrierInfo -> CarrierInfo
turnReverse (DUp,    row, col) = (DDown,  row, col)
turnReverse (DRight, row, col) = (DLeft,  row, col)
turnReverse (DDown,  row, col) = (DUp,    row, col)
turnReverse (DLeft,  row, col) = (DRight, row, col)

moveForward :: CarrierInfo -> CarrierInfo
moveForward (DUp,    row, col) = (DUp,    row - 1, col    )
moveForward (DRight, row, col) = (DRight, row    , col + 1)
moveForward (DDown,  row, col) = (DDown,  row + 1, col    )
moveForward (DLeft,  row, col) = (DLeft,  row    , col - 1)


type MBoard s = STUArray s (Int, Int) Word8

updateMBoard :: MBoard s -> (Int, Int) -> NodeState -> ST s ()
updateMBoard grid pos newState = do
  STUA.writeArray grid pos (toEnum . fromEnum $ newState)

simulate1 :: Board -> CarrierInfo -> Int -> Int
simulate1 (Board board) initialPosition iterations =
  let step :: STUArray s (Int, Int) Word8
           -> (CarrierInfo, Int)
           -> ST s (CarrierInfo, Int)
      step grid (!info, !infectionCount) = do
        current <- grid `STUA.readArray` carrierPos info
        case toEnum . fromEnum $ current of

          Clean -> do
            let info' = moveForward $ turnLeft info
            updateMBoard grid (carrierPos info) Infected
            return (info', infectionCount + 1)

          Infected -> do
            let info' = moveForward $ turnRight info
            updateMBoard grid (carrierPos info) Clean
            return (info', infectionCount)

          otherwise -> error
            $ "Unexpected cell value in simulate1: " ++ show current

  in runST $ do
    mboard <- STUA.thaw board :: ST s (STUArray s (Int, Int) Word8)
    (_, totalInfections) <- foldM (\state _ -> step mboard state)
                            (initialPosition, 0) [1..iterations]
    return totalInfections

  -- in totalInfections
  -- in map (\(_, board, _) -> board)
  --    $ take iterations
  --    $ iterate step (initialPosition, board, 0)

simulate2 :: Board -> CarrierInfo -> Int -> Int
simulate2 (Board board) initialPosition iterations =
  let step :: MBoard s
           -> (CarrierInfo, Int)
           -> ST s (CarrierInfo, Int)
      step grid (!info, !infectionCount) = do
        current <- grid `STUA.readArray` carrierPos info

        case toEnum . fromEnum $ current of

          Clean -> do
            let info' = moveForward $ turnLeft info
            updateMBoard grid (carrierPos info) Weakened
            return (info', infectionCount)

          Weakened -> do
            let info' = moveForward info
            updateMBoard grid (carrierPos info) Infected
            return (info', infectionCount + 1)

          Infected -> do
            let info' = moveForward $ turnRight info
            updateMBoard grid (carrierPos info) Flagged
            return (info', infectionCount)

          Flagged -> do
            let info' = moveForward $ turnReverse info
            updateMBoard grid (carrierPos info) Clean
            return (info', infectionCount)

  in runST $ do
    mboard <- STUA.thaw board :: ST s (STUArray s (Int, Int) Word8)
    (_, totalInfections) <- foldM (\state _ -> step mboard state)
                            (initialPosition, 0) [1..iterations]
    return totalInfections


main :: IO ()
main = do
  content <- getContents
  case parse (inputParser 10000) "<input>" content of
  -- case parse (inputParser 5) "<input>" content of
    Left error -> print error
    Right board -> do
      let (centerRow, centerCol) = boardCenter board

      print $ simulate1 board (DUp, centerRow, centerCol) 10000

      print $ simulate2 board (DUp, centerRow, centerCol) 10000000

      -- putStrLn
      --   $ intercalate "\n\n"
      --   $ map show
      --   $ simulate1 board (DUp, centerRow, centerCol) 70
