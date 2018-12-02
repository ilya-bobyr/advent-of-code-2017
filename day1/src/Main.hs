module Main where

main :: IO ()
main = do
  input <- getLine
  let digits = map (read . (: [])) input :: [Int]

      pairs1 = zip digits (tail digits ++ [head digits])

      halfSize = length digits `quot` 2
      pairs2 = zip digits $ drop halfSize digits ++ take halfSize digits

      answer p = sum $ map fst $ filter (uncurry (==)) p

  print $ answer pairs1
  print $ answer pairs2
