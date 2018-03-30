{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA


type Tape = UArray Int Bool

data State = StateA
           | StateB
           | StateC
           | StateD
           | StateE
           | StateF

data MoveDirection = MoveLeft
                   | MoveRight


type Transition = (State, Bool) -> (State, Bool, MoveDirection)

transition1 :: Transition

transition1 (StateA, False) = (StateB, True,  MoveRight)
transition1 (StateA, True)  = (StateC, False, MoveLeft)

transition1 (StateB, False) = (StateA, True,  MoveLeft)
transition1 (StateB, True)  = (StateC, True,  MoveRight)

transition1 (StateC, False) = (StateA, True,  MoveRight)
transition1 (StateC, True)  = (StateD, False, MoveLeft)

transition1 (StateD, False) = (StateE, True,  MoveLeft)
transition1 (StateD, True)  = (StateC, True,  MoveLeft)

transition1 (StateE, False) = (StateF, True,  MoveRight)
transition1 (StateE, True)  = (StateA, True,  MoveRight)

transition1 (StateF, False) = (StateA, True,  MoveRight)
transition1 (StateF, True)  = (StateE, True,  MoveRight)


transitionTest :: Transition

transitionTest (StateA, False) = (StateB, True, MoveRight)
transitionTest (StateA, True)  = (StateB, False, MoveLeft)

transitionTest (StateB, False) = (StateA, True, MoveLeft)
transitionTest (StateB, True)  = (StateA, True, MoveRight)


movePos :: MoveDirection -> Int -> Int
movePos MoveLeft pos = pos - 1
movePos MoveRight pos = pos + 1


diagnosticChecksum :: Tape -> Int
diagnosticChecksum tape =
  sum $ map (\v -> if v then 1 else 0) $ UA.elems tape


data ExtensionSide = ExtendOnLeft
                   | ExtendOnRight

extendTape :: ExtensionSide -> Int -> Tape -> Tape
extendTape side count tape =
  let (l, u) = UA.bounds tape
  in case side of
       ExtendOnLeft ->
         UA.listArray (l - count, u)
         $ replicate count False ++ UA.elems tape
       ExtendOnRight ->
         UA.listArray (l, u + count)
         $ UA.elems tape ++ replicate count False

extendTapeIfNecessary :: Int -> Tape -> Int -> Tape
extendTapeIfNecessary count tape pos =
  let (l, u) = UA.bounds tape
      tape' | pos <= l = extendTape ExtendOnLeft count tape
            | pos >= u = extendTape ExtendOnRight count tape
            | otherwise = tape
  in tape'


run :: Transition -> (State, Int) -> Int
run transition (initialState, checksumAfter) =
  let go :: Int -> Tape -> State -> Int -> Int
      go 0 tape _ _ = diagnosticChecksum tape
      go !count tape state pos =
        let value = tape UA.! pos
            (state', value', dir) = transition (state, value)
            pos' = movePos dir pos
            tape' = extendTapeIfNecessary 100 tape pos
                    UA.// [(pos, value')]
        in go (count - 1) tape' state' pos'

      emptyTape = UA.listArray (-100, 100) $ repeat False
  in go checksumAfter emptyTape initialState 0


solve1 :: Int
solve1 = run transition1 (StateA, 12134527)

solveTest :: Int
solveTest = run transitionTest (StateA, 6)



main :: IO ()
main = do
  -- print $ solveTest
  print $ solve1
