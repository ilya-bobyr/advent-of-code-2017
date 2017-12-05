module Main where

main :: IO ()
main = do
  input <- getContents
  let spreadsheet = map ((map read) . words) $ lines input :: [[Int]]

      checksum1 = sum $ map (\l -> maximum l - minimum l) spreadsheet

      dividable = map (\l ->
                         filter (\e ->
                                   any (\e2 -> e /= e2
                                         && (    e `rem` e2 == 0
                                             || e2 `rem` e  == 0)) l) l)
                      spreadsheet
      checksum2 = sum $ map (\[a, b] -> if a `rem` b == 0
                                        then a `quot` b
                                        else b `quot` a) dividable

  print checksum1
  print checksum2
