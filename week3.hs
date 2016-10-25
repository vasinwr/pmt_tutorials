import Data.Monoid

isPalindrome (xs)
  = check xs xs []
  where 
    check (x:xs) (y1:y2:ys) acc 
      = check xs ys (x:acc)
    check (x:xs) [] (a:acc)
      | x == a = check xs [] acc
      | otherwise = False
    check [] [] [] = True

maxSeqSum xs 
  = mSS xs 0 []
  where 
    mSS (x:xs) max stack
      | x < 0 = mSS xs max (x:stack)
      | x > 0 && null stack = mSS xs (max + x) []
      | x + sum stack > 0 = mSS xs (max + x + sum stack) []
      | x < max = mSS xs max []
      | otherwise = mSS xs x []
    mSS [] max stack
      = max
