module Day1 where


day1 :: String -> Int
day1 = sum . map openClose
  where 
    openClose '(' = 1
    openClose ')' = -1
    openClose _   = 0


basementPos :: String -> Int
basementPos str = bp str 0 0
  where
    bp (')':xs) 0 i = i + 1
    bp (')':xs) n i = bp xs (n-1) (i+1)
    bp ('(':xs) n i = bp xs (n+1) (i+1)
    bp _ _ _ = 0

