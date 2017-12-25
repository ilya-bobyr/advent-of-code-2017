{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (read)
import qualified Prelude as P
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed.Mutable (MVector, new, read, write)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Int (Int32)


steps :: (PrimMonad m) => (Int -> Int) -> [Int] -> m Int
steps adjuster js = do
  maze <- new $ length js
  init maze js 0
  process maze 0 0
  where init maze (j:js) !i = do write maze i j
                                 init maze js (i+1)
        init maze [] _ = return ()

        process maze !pos !count | pos < 0
                                   || pos >= MV.length maze = return count
        process maze !pos !count = do offset <- read maze pos
                                      write maze pos $ adjuster offset
                                      process maze (pos + offset) (count + 1)

main :: IO ()
main = do
  input <- getContents
  let jumps = fmap P.read $ lines input :: [Int]
  res1 <- steps (\v -> v + 1) jumps
  print res1
  res2 <- steps (\v -> if v >= 3
                       then v - 1
                       else v + 1) jumps
  print res2
