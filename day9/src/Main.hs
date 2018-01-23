{-# LANGUAGE BangPatterns #-}

module Main where

stripGarbage :: String -> (Int, String)
stripGarbage cs = outside 0 "" cs
  where outside !t !acc ('<':cs) = inside t acc cs
        outside !t !acc (c:cs) = outside t (c:acc) cs
        outside !t !acc [] = (t, reverse acc)

        inside !t !acc ('!':_:cs) = inside t acc cs
        inside !t !acc ('>':cs) = outside t acc cs
        inside !t !acc (c:cs) = inside (t+1) acc cs
        inside !t !acc [] = (t, reverse acc)

sumGroups :: String -> Either (String, String) Int
sumGroups cs = go 0 0 cs
  where go !total !stack ('{':cs) = go (total+stack+1) (stack+1) cs
        go !total !stack ('}':cs) = if stack == 0
                                    then Left ( "Unmatched close bracket"
                                              , ('}':cs))
                                    else go total (stack-1) cs
        go !total !stack (',':cs) = go total stack cs
        go !total !stack (c:cs) = Left ("Unexpected character ", (c:cs))
        go !total !stack [] = if stack /= 0
                              then Left ("Unbalanced: " ++ show stack, "")
                              else Right total

main :: IO ()
main = do
  input <- getLine
  let (totalGarbage, noGarbage) = stripGarbage input
  print $ sumGroups noGarbage
  print totalGarbage
