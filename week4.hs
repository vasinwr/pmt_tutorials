-- power set
-- input: set (list of elem of type a) 
-- output powerset (list of lists of elem of type a)
powerSet::[a] -> [[a]]
powerSet xs 
  = powerSet' xs [[]] 
  where  
    powerSet' [] acc
      = acc
    powerSet' (x:xs) acc
      = powerSet' xs ([(x:a)| a<-acc] ++ acc)

-- a cleaner version done in class :)
powerSet2 :: [a] -> [[a]]
powerSet2 []
  = [[]]
powerSet2 (x:xs) 
  = (map (x:) subsets) ++ (subsets)
  where
    subsets = powerSet2 xs
