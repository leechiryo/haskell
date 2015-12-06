-- problem 41
import Data.List

divisors :: Int -> [Int]
divisors n = let cs = [1..floor $ sqrt (fromIntegral n :: Double)]
             in (map head . group . sort) $ concat $ map (\x -> if n `mod` x == 0 then [x, n `div` x] else []) cs

primeHelper :: Int -> Int -> Int -> Bool
primeHelper n d max = if d > max
                      then True
                      else if n `mod` d == 0 
                           then False
                           else primeHelper n (d+1) max

isPrime :: Int -> Bool
isPrime n = let max = floor $ sqrt (fromIntegral n)
            in primeHelper n 2 max

permute :: Eq a => [a] -> Int -> [[a]]
permute a 1 = map (\x -> [x]) a
permute a n = concat $ map (\x -> (map (\y -> x:y) (permute (filter (/=x) a) (n-1)))) a

-- create the candidate pandigital numbers.
-- we tried all of the 1 to 9 and 1 to 8 pandigital numbers and there are not prime.
-- so, use the 1 to 7 pandigital numbers to be candidate.
createCandidate :: [String]
createCandidate = reverse $ sort $ filter (\x -> (last x) `elem` "13579") $ permute "1234567" 7

-- search the candidate list from big to small, it will find a prime number then it will 
-- ignore the following candidates
answer :: Int
answer = foldl (\acc x -> if acc > 2 then acc else if isPrime $ read x then read x else 2) 2 createCandidate
--output 7652413
