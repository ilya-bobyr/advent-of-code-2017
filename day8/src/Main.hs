module Main where

import Control.Monad (liftM)
import qualified Data.Map.Strict as M

import Data.Either (lefts, rights)
import Data.Foldable as F

import Text.Parsec (parse, (<|>))
import Text.Parsec.Prim (many, try)
import Text.Parsec.Char (noneOf, oneOf, char, digit, string, newline)
import Text.Parsec.Combinator (many1, option, eof)
import Text.Parsec.String (Parser)

import Numeric (readDec)


type VarName = String
type VarTable = M.Map VarName Int
type Condition = VarTable -> Bool
type Command = (VarName, Int, Condition)

hspaces :: Parser String
hspaces = many $ oneOf " \t"

inputParser :: Parser [Command]
inputParser = do result <- many commandParser
                 eof
                 return result

commandParser :: Parser Command
commandParser = do var <- many1 $ noneOf " \t"
                   hspaces
                   op <-     string "inc"
                         <|> string "dec"
                   hspaces
                   sign <- option '+' (char '-')
                   [(uval, "")] <- liftM readDec $ many1 digit
                   let val = if sign == '+' then uval else (-uval)
                   hspaces >> string "if" >> hspaces
                   condition <- conditionParser
                   hspaces
                   newline
                   return ( var
                          , if op == "inc" then val else (-val)
                          , condition)

conditionParser :: Parser Condition
conditionParser = do var <- many1 $ noneOf " \t"
                     hspaces
                     op <-     (try $ do string "=="
                                         return (==))
                           <|> (try $ do string "!="
                                         return (/=))
                           <|> (try $ do string "<="
                                         return (<=))
                           <|> (try $ do string ">="
                                         return (>=))
                           <|> (try $ do string "<"
                                         return (<))
                           <|> (try $ do string ">"
                                         return (>))
                     hspaces
                     sign <- option '+' (char '-')
                     [(uval, "")] <- liftM readDec $ many1 digit
                     let val = if sign == '+' then uval else (-uval)
                     return $ \t -> case M.lookup var t of
                                      Just actual -> op actual val
                                      Nothing -> op 0 val

main :: IO ()
main = do
  content <- getContents
  case parse inputParser "<input>" content of
    Left error -> print error
    Right commands -> do
      let perform val (Just v) = Just $ v + val
          perform val Nothing = Just val

          (vars, maxEver) =
            foldl' (\(vars, m) (name, val, cond) ->
                       if cond vars
                       then let vars' = M.alter (perform val) name vars
                            in (vars'
                               , max m $ M.findWithDefault 0 name vars')
                       else (vars, m))
                   (M.empty, 0) commands
      print $ F.maximum vars
      print maxEver
