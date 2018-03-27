{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>), (<*>), (<*))
import Data.Numbers.Primes (wheelSieve)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Text.Parsec (parse, (<|>))
import Text.Parsec.Prim (many, try, parserFail)
import Text.Parsec.Char (char, oneOf, noneOf, digit, letter, string, newline)
import Text.Parsec.Combinator (many1, sepBy, option, eof)
import Text.Parsec.String (Parser)


newtype Register = Reg Char
                 deriving (Show)

data Value = Var Register
           | Literal Int
           deriving (Show)

data Command = Set Register Value
             | Sub Register Value
             | Mul Register Value
             | JumpNZ Value Value
             deriving (Show)


inputParser :: Parser [Command]
inputParser = do
  result <- many lineParser
  eof
  return result

hspaces :: Parser String
hspaces = many $ oneOf " \t"

parseRegister :: Parser Register
parseRegister = do
  reg <- letter
  return $ Reg reg

parseLiteral :: Parser Int
parseLiteral = do
  sign <- option '+' $ (char '-' <* hspaces)
  v <- read <$> many1 digit
  return $ if sign == '+'
           then v
           else (-v)

parseValue :: Parser Value
parseValue = do
  try (Var <$> parseRegister)
    <|> Literal <$> parseLiteral

lineParser :: Parser Command
lineParser = do
  name <- many1 $ noneOf " "
  hspaces
  let twoArgs cmd xParser yParser = do
        x <- xParser
        hspaces
        y <- yParser
        return $ cmd x y
  res <- case name of
           "set" -> twoArgs Set parseRegister parseValue
           "sub" -> twoArgs Sub parseRegister parseValue
           "mul" -> twoArgs Mul parseRegister parseValue
           "jnz" -> twoArgs JumpNZ parseValue parseValue
           otherwise -> parserFail $ "Unexpected command: " ++ show name
  newline
  return res


type Program = V.Vector Command
type Env = Map.Map Char Int


solve1 :: Program -> Int
solve1 prog =
  let progLength = V.length prog

      run :: Int -> Int -> Env -> Int
      run !i !mulCount env =
        let done = i < 0 || i >= progLength
            cmd = prog V.! i
        in if done
           then mulCount
           else eval cmd i mulCount env

      readReg env (Reg reg) = Map.findWithDefault 0 reg env

      read env (Var reg) = readReg env reg
      read _ (Literal v) = v

      update env (Reg reg) f =
        let prev = Map.findWithDefault 0 reg env
        in Map.insert reg (f prev) env

      eval (Set reg value) !i !mulCount env =
        run (i + 1) mulCount $
          update env reg (\_ -> read env value)

      eval (Sub reg value) !i !mulCount env =
        run (i + 1) mulCount $
          update env reg (\prev -> prev - read env value)

      eval (Mul reg value) !i !mulCount env =
        run (i + 1) (mulCount + 1) $
          update env reg (\prev -> prev * read env value)

      eval (JumpNZ xValue yValue) !i !mulCount env =
        let x = read env xValue
            y = read env yValue
            i' | x /= 0    = i + y
               | otherwise = i + 1
        in run i' mulCount env

  in run 0 0 Map.empty

solve2' :: Program -> Int -> Int
solve2' prog maxCycles =
  let progLength = V.length prog

      run :: Int -> Int -> Env -> Int
      run !i !cycle env =
        let done = cycle >= maxCycles
              || i < 0
              || i >= progLength
            cmd = prog V.! i
        in if done
           then readReg env (Reg 'h')
           else eval cmd i cycle env

      readReg env (Reg reg) = Map.findWithDefault 0 reg env

      read env (Var reg) = readReg env reg
      read _ (Literal v) = v

      update env (Reg reg) f =
        let prev = Map.findWithDefault 0 reg env
        in Map.insert reg (f prev) env

      eval (Set reg value) !i !cycle env =
        run (i + 1) (cycle + 1) $
          update env reg (\_ -> read env value)

      eval (Sub reg value) !i !cycle env =
        run (i + 1) (cycle + 1) $
          update env reg (\prev -> prev - read env value)

      eval (Mul reg value) !i !cycle env =
        run (i + 1) (cycle + 1) $
          update env reg (\prev -> prev * read env value)

      eval (JumpNZ xValue yValue) !i !cycle env =
        let x = read env xValue
            y = read env yValue
            i' | x /= 0    = i + y
               | otherwise = i + 1
        in run i' (cycle + 1) env

  in run 0 0 $ Map.singleton 'a' 1

primesRange :: (Int, Int) -> [Int]
primesRange (from, to) =
  takeWhile (<= to)
  $ dropWhile (< from)
  $ wheelSieve 6

solve2 :: (Int, Int) -> Int -> Int
solve2 (from, to) step =
  let primes = primesRange (from, to)
      candidates = [from, from + step .. to]

      count :: ([Int], [Int], Int) -> Int
      count (cs@(c:cs'), ps@(p:ps'), !res)
        | c < p     = count (cs', ps, res + 1)
        | c == p    = count (cs', ps', res)
        | otherwise = count (cs, ps', res)
      count (cs, [], res) = res + length cs
      count ([], _, res) = res

  in count (candidates, primes, 0)

main :: IO ()
main = do
  content <- getContents
  case parse inputParser "<input>" content of
    Left error -> print error
    Right commands -> do
      let prog = V.fromList commands
      print $ solve1 prog
      -- print $ solve2 prog 1000000000
      print $ solve2 (107900, 124900) 17
