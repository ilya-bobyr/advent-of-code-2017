{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>), (<*))
import Data.List (sortBy, takeWhile, intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Int (Int64)
import Text.Parsec (parse)
import Text.Parsec.Prim (many)
import Text.Parsec.Char (char, string, digit, newline, oneOf)
import Text.Parsec.Combinator (many1, option, eof)
import Text.Parsec.String (Parser)
import Text.Printf (printf)


type Vec3d = (Int64, Int64, Int64)

data Particle = Particle {
    partPos :: Vec3d,
    partVel :: Vec3d,
    partAcc :: Vec3d
  }
  deriving (Show, Eq)


inputParser :: Parser [Particle]
inputParser = do
  result <- many lineParser
  eof
  return result

hspaces :: Parser String
hspaces = many $ oneOf " \t"

numberParser :: Parser Int64
numberParser = do
  sign <- option '+' (char '-')
  v <- read <$> many1 digit
  return $ if sign == '+'
           then v
           else (-v)

token :: String -> Parser String
token str =
  hspaces >> string str <* hspaces

lineParser :: Parser Particle
lineParser = do
  token "p=<"
  px <- numberParser
  token ","
  py <- numberParser
  token ","
  pz <- numberParser
  token ">,"
  token "v=<"
  vx <- numberParser
  token ","
  vy <- numberParser
  token ","
  vz <- numberParser
  token ">,"
  token "a=<"
  ax <- numberParser
  token ","
  ay <- numberParser
  token ","
  az <- numberParser
  token ">"
  newline
  return $ Particle (px, py, pz) (vx, vy, vz) (ax, ay, az)


solve1 :: [(Int, Particle)] -> [Int]
solve1 indexedParticles =
  let
    square :: (Int, Particle) -> Int64
    square (_, (Particle _ _ (ax, ay, az))) = ax^2 + ay^2 + az^2

    orderedByAcc :: [(Int, Particle)]
    orderedByAcc = sortBy (\a b -> compare (square a) (square b))
                          indexedParticles

    slowest :: [(Int, Particle)]
    slowest = takeWhile (\v-> square v == square (head orderedByAcc))
                        orderedByAcc
  in map fst $ slowest


-- solve2 :: [Particle] -> [Particle]
solve2 :: [Particle] -> Int
solve2 particles =
  let -- orderByAcc :: [Particle] -> [Particle]
      -- orderByAcc particles =
      --   let sq (x, y, z) = x^2 + y^2 + z^2
      --   in sortBy (\p1 p2 ->
      --                let p1a = sq $ partAcc p1
      --                    p2a = sq $ partAcc p2
      --                    p1v = sq $ partVel p1
      --                    p2v = sq $ partVel p2
      --                    p1p = sq $ partPos p1
      --                    p2p = sq $ partPos p2
      --                in if p1a /= p2a
      --                   then compare p1a p2a
      --                   else if p1v /= p2v
      --                        then compare p1v p2v
      --                        else compare p1p p2p
      --             ) particles

      tickParticle :: Particle -> Particle
      tickParticle (Particle (!px, !py, !pz) (!vx, !vy, !vz)
                    (!ax, !ay, !az)) =
        Particle (px + vx + ax, py + vy + ay, pz + vz + az)
                 (vx + ax, vy + ay, vz + az) (ax, ay, az)

      tick :: [Particle] -> [Particle]
      tick !particles =
        let particlesMoved = map tickParticle particles
            colliding = M.keysSet
                        $ M.filter (>1)
                        $ M.fromListWith (+)
                        $ map (\p -> (partPos p, 1)) particlesMoved
            doesNotCollide p = S.notMember (partPos p) colliding
        in filter doesNotCollide particlesMoved
        -- in orderByAcc $ filter doesNotCollide particlesMoved

      -- allInOneDirectionOrder :: [Particle] -> Bool
      -- allInOneDirectionOrder particles =
      --   let axisInOrder :: (Int, Int, Int) -> Bool
      --       axisInOrder (p, v, a) =
      --            (a > 0 && v > 0 && p > 0)
      --         || (a == 0 && (   (v > 0 && p > 0)
      --                        || (v == 0)
      --                        || (v < 0 && p < 0)))
      --         || (a < 0 && v < 0 && p < 0)

      --       inOneDirection :: Particle -> Bool
      --       inOneDirection (Particle (px, py, pz) (vx, vy, vz) (ax, ay, az)) =
      --         axisInOrder (px, vx, ax) && axisInOrder (py, vy, ay)
      --         && axisInOrder (pz, vz, az)
      --   in all $ map inOneDirection particles

      -- allInOneDirection = until allInOneDirectionOrder tick orderedByAcc

      -- noInteractionOrder :: Particle -> Particle -> Bool
      -- noInteractionOrder
      --   (Particle (p1x, p1y, p1z) (v1x, v1y, v1z) (a1x, a1y, a1z))
      --   (Particle (p2x, p2y, p2z) (v2x, v2y, v2z) (a2x, a2y, a2z)) =
      --   let sq (x, y, z) = x^2 + y^2 + z^2
      --   in     sq (a1x, a1y, a1z) < sq (a2x, a2y, a2z)
      --       && sq (v1x, v1y, v1z) < sq (v2x, v2y, v2z)
      --       && sq (p1x, p1y, p1z) < sq (p2x, p2y, p2z)

      -- stop :: [Particle] -> Bool
      -- stop particles =
      --   and $ zipWith noInteractionOrder particles (tail particles)

  -- What is the right stop criteria?

  -- in length $ until stop tick particles
  in length $ iterate tick particles !! 10000
  -- in until stop tick particles

showParticleAndSquares :: Particle -> String
showParticleAndSquares (Particle (px, py, pz) (vx, vy, vz) (ax, ay, az)) =
  printf (    "a: %6d <- (%4d, %4d, %4d)"
         ++ ", v: %12d <- (%8d, %8d, %8d)"
         ++ ", p: %20d <- (%12d, %12d, %12d)")
         (ax^2 + ay^2 + az^2) ax ay az
         (vx^2 + vy^2 + vz^2) vx vy vz
         (px^2 + py^2 + pz^2) px py pz


main :: IO ()
main = do
  content <- getContents
  case parse inputParser "<input>" content of
    Left error -> print error
    Right particles -> do
      print $ solve1 $ zip [0..] particles
      print $ solve2 particles
      -- putStrLn $ intercalate "\n" $ map showParticleAndSquares $ solve2 particles
