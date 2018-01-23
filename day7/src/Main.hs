{-# LANGUAGE BangPatterns #-}

module Main where

import Text.Parsec (parse, (<|>))
import Text.Parsec.Prim (many, try)
import Text.Parsec.Char (noneOf, oneOf, char, digit, string, newline)
import Text.Parsec.Combinator (many1, sepBy, eof)
import Text.Parsec.String (Parser)
import Numeric (readDec)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tree (rootLabel, subForest)
import qualified Data.Tree as T
import Data.Either (lefts, rights)

inputParser :: Parser [(String, Int, [String])]
inputParser = do result <- many diskParser
                 eof
                 return result

hspaces :: Parser String
hspaces = many $ oneOf " \t"

diskParser :: Parser (String, Int, [String])
diskParser = do name <- many1 $ noneOf " \t\n"
                hspaces >> char '(' >> hspaces
                weightStr <- many1 digit
                let [(weight, "")] = readDec weightStr
                hspaces >> char ')' >> hspaces
                children <- try (do string "->" >> hspaces
                                    many1 (noneOf ", \t\n")
                                      `sepBy` (hspaces >> char ',' >>
                                               hspaces))
                            <|> return []
                hspaces
                newline
                return (name, weight, children)

main :: IO ()
main = do
  content <- getContents
  let Right connections = parse inputParser "<input>" content

      nodes = foldr (\c@(name, _, _) map -> M.insert name c map)
                    M.empty connections

      children = foldr (\(_, _, children) set ->
                          foldr (\name set -> S.insert name set)
                                set children)
                    S.empty connections

      roots = filter (\(name, _, _) -> S.notMember name children)
                     connections
      [root] = roots
      (rootName, _, _) = root

  print rootName

  let tree = T.unfoldTree (\(name, weight, children) ->
                             ((name, weight),
                              map (\(Just c) -> c) $
                              map (\n -> M.lookup n nodes) children))
                          root

      treeWithChildWeight = sumChildren tree

      sumChildren T.Node{ rootLabel=(name, weight)
                        , subForest=children } =
        let children' = map sumChildren children
            childrenWeight =
              sum $ map ((\(_, _, w) -> w)
                         . rootLabel) children'
        in T.Node{ rootLabel=(name, weight, weight + childrenWeight)
                 , subForest=children' }

      res2 = T.foldTree findImbalance treeWithChildWeight

      sameWeight [] = True
      sameWeight [_] = True
      sameWeight ((_, _, w) : ws) =
        all (\(_, _, w') -> w == w') ws

      findImbalance a children
        | lefts children /= [] = Left $ head $ lefts children
      findImbalance a children =
        let children' = rights children
        in if sameWeight children'
           then Right a
           else Left children'

  print res2
