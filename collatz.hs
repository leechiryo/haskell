import qualified Data.Map as Map

unboxJust :: Maybe Integer -> Integer
unboxJust Nothing = 0
unboxJust (Just n) = n

collatz :: Integer -> Map.Map Integer Integer -> (Integer, Map.Map Integer Integer)
collatz 1 m = (1, Map.insert 1 1 m)
collatz n m = if Map.member n m 
               then (unboxJust $ Map.lookup n m, m)
               else 
                  if n `mod` 2 == 0
                  then let s = collatz (n `div` 2) m in (1 + (fst s), Map.insert n (1 + (fst s)) $ snd s)
                  else let s = collatz (n * 3 + 1) m in (1 + (fst s), Map.insert n (1 + (fst s)) $ snd s)

collatzMax :: [Integer] -> Map.Map Integer Integer -> (Integer, Integer, Map.Map Integer Integer)
collatzMax [] m = (0, 0, m)
collatzMax (n:es) m = 
  let (cnt, newm) = collatz n m
      (newn, max, newm2) = collatzMax es newm
  in if cnt > max
    then (n, cnt, newm2) 
    else (newn, max, newm2)

main = putStrLn $ show $ let (a,b,c) = collatzMax [1..1000000] Map.empty in (a,b)

