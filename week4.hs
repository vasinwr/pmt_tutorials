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
