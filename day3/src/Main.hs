{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (read, replicate)
import qualified Prelude as P
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed.Mutable (MVector, new, read, write)
import Data.Int (Int32)
import Data.Foldable (sum)
import Control.Applicative (liftA)

type Pos = (Int, Int)

type Field m = MVector (PrimState m) Int32


sizes :: [Int]
sizes = mconcat $ map (\[i] -> [i, i]) $ fmap (:[]) [1, 2..]

type OneStep = (Int, Int) -> (Int, Int)
dirs :: [OneStep]
dirs = cycle [ (\(x, y) -> (x+1, y))
             , (\(x, y) -> (x  , y-1))
             , (\(x, y) -> (x-1, y))
             , (\(x, y) -> (x  , y+1))
             ]

type ValueGen m = Field m -> Pos -> Pos -> m Int32
type ContinuePred m = Field m -> Pos -> m Bool
type ResultF m a = Field m -> Pos -> m a

fieldSize :: Int
fieldSize = 2000

halfFieldSize :: Int
halfFieldSize = fieldSize `quot` 2

posToIndex :: Pos -> Int
posToIndex (x, y) |    x > -halfFieldSize && x < halfFieldSize
                    && y > -halfFieldSize && y < halfFieldSize
  = (y + halfFieldSize) * fieldSize + x + halfFieldSize
posToIndex _ = error "x or y is out of range"

readPos :: (PrimMonad m) => Field m -> Pos -> m Int32
readPos f p = read f (posToIndex p)

calculate :: (PrimMonad m) => (ValueGen m, ContinuePred m, ResultF m a) -> m a
calculate (vg, cp, rf) = do
  field <- new (fieldSize * fieldSize)
  write field (posToIndex (0, 0)) 1
  (field, pos) <- go field (0, 0) sizes dirs
  rf field pos
    where go f p (s:ss) (d:ds) = do
            continue <- cp f p
            if continue
            then do let p' = d p
                    v <- vg f p p'
                    write f (posToIndex p') v
                    if s > 1
                    then go f p' (s-1:ss) (d:ds)
                    else go f p' ss ds
            else return $ (f, p)

main :: IO ()
main = do
  target <- fmap P.read getLine :: IO Int
  (x, y) <- calculate ( (\f p _ -> do
                            v <- readPos f p
                            return $ v + 1
                        )
                      , (\f p -> do
                            v <- readPos f p
                            return $ v < fromIntegral target
                        )
                      , (\_ p -> return p)
                      )
  print $ abs x + abs y
  res2 <- calculate ( (\f _ (x, y) -> do
                          v <- liftA sum $ sequence [
                              readPos f (x - 1, y - 1)
                            , readPos f (x    , y - 1)
                            , readPos f (x + 1, y - 1)
                            , readPos f (x - 1, y    )
                            , readPos f (x + 1, y    )
                            , readPos f (x - 1, y + 1)
                            , readPos f (x    , y + 1)
                            , readPos f (x + 1, y + 1)
                            ]
                          return v
                      )
                    , (\f p -> do
                          v <- readPos f p
                          return $ v < fromIntegral target
                      )
                    , (\f p -> readPos f p)
                    )
  print res2
