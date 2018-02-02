module Main where

import Control.Applicative ((<$>))
import Data.List (dropWhile)
import Text.Parsec (parse)
import Text.Parsec.Prim (many, try)
import Text.Parsec.Char (char, oneOf, digit, string, newline)
import Text.Parsec.Combinator (many1, sepBy, eof)
import Text.Parsec.String (Parser)

inputParser :: Parser [(Int, Int)]
inputParser = do
  result <- many lineParser
  eof
  return result

hspaces :: Parser String
hspaces = many $ oneOf " \t"

lineParser :: Parser (Int, Int)
lineParser = do layer <- read <$> many digit
                hspaces >> char ':' >> hspaces
                range <- read <$> many digit
                newline
                return (layer, range)

solve1 :: [(Int, Int)] -> Int
solve1 layers = severity 0 layers
  where
    severity sum [] = sum
    severity sum ((i, range) : ls) =
      if i `rem` ((range - 1) * 2) == 0
      then severity (sum + i * range) ls
      else severity sum ls

solve2 :: [(Int, Int)] -> Int
solve2 layers =
  head $ dropWhile (\s -> not $ checkPass layers s) [0..]
  where
    checkPass [] _ = True
    checkPass ((i, range) : ls) s =
      if (i + s) `rem` ((range - 1) * 2) == 0
      then False
      else checkPass ls s

main :: IO ()
main = do
  content <- getContents
  case parse inputParser "<input>" content of
    Left error -> print error
    Right layers -> do
      print $ solve1 layers
      print $ solve2 layers
