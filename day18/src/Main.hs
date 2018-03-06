{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>), (<*>), (<*))
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

data Command = Send Value
             | Set Register Value
             | Add Register Value
             | Mul Register Value
             | Mod Register Value
             | Receive Register
             | JumpGZ Value Value
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
           "snd" -> Send <$> parseValue
           "set" -> twoArgs Set parseRegister parseValue
           "add" -> twoArgs Add parseRegister parseValue
           "mul" -> twoArgs Mul parseRegister parseValue
           "mod" -> twoArgs Mod parseRegister parseValue
           "rcv" -> Receive <$> parseRegister
           "jgz" -> twoArgs JumpGZ parseValue parseValue
           otherwise -> parserFail $ "Unexpected command: " ++ show name
  newline
  return res


type Program = V.Vector Command
type Env = Map.Map Char Int


solve1 :: Program -> Int
solve1 prog = run 0 0 Map.empty
  where
    run :: Int -> Int -> Env -> Int
    run !i !lastSend env =
      let cmd = prog V.! i
      in eval cmd i lastSend env

    readReg env (Reg reg) = Map.findWithDefault 0 reg env

    read env (Var reg) = readReg env reg
    read _ (Literal v) = v

    update env (Reg reg) f =
      let prev = Map.findWithDefault 0 reg env
      in Map.insert reg (f prev) env

    eval (Send value) !i !lastSend env =
      run (i + 1) (read env value) env

    eval (Set reg value) !i !lastSend env =
      run (i + 1) lastSend $
        update env reg (\_ -> read env value)

    eval (Add reg value) !i !lastSend env =
      run (i + 1) lastSend $
        update env reg (\prev -> prev + read env value)

    eval (Mul reg value) !i !lastSend env =
      run (i + 1) lastSend $
        update env reg (\prev -> prev * read env value)

    eval (Mod reg value) !i !lastSend env =
      run (i + 1) lastSend $
        update env reg (\prev -> prev `mod` read env value)

    eval (Receive reg) !i !lastSend env =
      if readReg env reg /= 0
      then lastSend
      else run (i + 1) lastSend env

    eval (JumpGZ xValue yValue) !i !lastSend env =
      let x = read env xValue
          y = read env yValue
      in if x > 0
         then run (i + y) lastSend env
         else run (i + 1) lastSend env


data EvalResult = ProgramTerminated
                | ProgramBlocked

type ProgramState = ( Int -- next instruction index
                    , Env -- variable environment
                    , [Int] -- input queue
                    , Int -- send counter
                    )

solve2 :: Program -> Int
solve2 prog =
  let p1_initialState = initialState 0 []
      (p2_input, p1, p1_res) = run p1_initialState []
      p2_initialState = initialState 1 p2_input
      (p1_input, p2, p2_res) = run p2_initialState []

  in runBoth ((updateInput p1 p1_input), p1_res) (p2, p2_res)

  where
    initialState :: Int -> [Int] -> ProgramState
    initialState id input = (0, Map.singleton 'p' id, input, 0)

    updateInput :: ProgramState -> [Int] -> ProgramState
    updateInput (i, env, oldInput, count) newInput =
      (i, env, oldInput ++ newInput, count)

    runBoth :: (ProgramState, EvalResult)
            -> (ProgramState, EvalResult)
            -> Int
    runBoth _ ((_, _, _, sendCount), ProgramTerminated) = sendCount

    runBoth ((_, _, [], _), ProgramBlocked)
      ((_, _, [], sendCount), ProgramBlocked) = sendCount

    runBoth (_, ProgramTerminated)
      ((_, _, [], sendCount), ProgramBlocked) = sendCount

    runBoth (p1@(_, _, [], _), p1_res) (p2, _) =
      let (p1_newInput, p2', p2_res) = run p2 []
          p1' = updateInput p1 p1_newInput
      in runBoth (p1', p1_res) (p2', p2_res)

    runBoth (p1, ProgramTerminated) (p2, _) =
      let (p1_newInput, p2', p2_res) = run p2 []
          p1' = updateInput p1 p1_newInput
      in runBoth (p1', ProgramTerminated) (p2', p2_res)

    runBoth (p1, ProgramBlocked) (p2, p2_res) =
      let (p2_newInput, p1', p1_res) = run p1 []
          p2' = updateInput p2 p2_newInput
      in runBoth (p1', p1_res) (p2', p2_res)

    run :: ProgramState -> [Int] -> ([Int], ProgramState, EvalResult)
    run ps@(!i, !env, !input, !sendCount) output =
      if i < 0 || i >= V.length prog
      then (output, ps, ProgramTerminated)
      else let cmd = prog V.! i
           in eval cmd i env input sendCount output

    readReg env (Reg reg) = Map.findWithDefault 0 reg env

    read env (Var reg) = readReg env reg
    read _ (Literal v) = v

    update env (Reg reg) f =
      let prev = Map.findWithDefault 0 reg env
      in Map.insert reg (f prev) env

    eval (Send value) !i !env !input !sendCount !output =
      let output' = read env value : output
          ps' = (i + 1, env, input, sendCount + 1)
      in run ps' output'

    eval (Set reg value) !i !env !input !sendCount !output =
      let env' = update env reg (\_ -> read env value)
          ps' = (i + 1, env', input, sendCount)
      in run ps' output

    eval (Add reg value) !i !env !input !sendCount !output =
      let env' = update env reg (\prev -> prev + read env value)
          ps' = (i + 1, env', input, sendCount)
      in run ps' output

    eval (Mul reg value) !i !env !input !sendCount !output =
      let env' = update env reg (\prev -> prev * read env value)
          ps' = (i + 1, env', input, sendCount)
      in run ps' output

    eval (Mod reg value) !i !env !input !sendCount !output =
      let env' = update env reg (\prev -> prev `mod` read env value)
          ps' = (i + 1, env', input, sendCount)
      in run ps' output

    eval (Receive reg) !i !env [] !sendCount !output =
      (reverse output, (i, env, [], sendCount), ProgramBlocked)

    eval (Receive reg) !i !env (value:input') !sendCount !output =
      let env' = update env reg (\_ -> value)
          ps' = (i + 1, env', input', sendCount)
      in run ps' output

    eval (JumpGZ xValue yValue) !i !env !input !sendCount !output =
      let x = read env xValue
          y = read env yValue
          ps' = (if x > 0 then i + y else i + 1
                , env, input, sendCount)
      in run ps' output


main :: IO ()
main = do
  content <- getContents
  case parse inputParser "<input>" content of
    Left error -> print error
    Right commands -> do
      print $ solve1 (V.fromList commands)
      print $ solve2 (V.fromList commands)
