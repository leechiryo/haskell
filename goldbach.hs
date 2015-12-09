-- problem 46
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

candidate :: [Int]
candidate = filter (\x -> x `mod` 2 /= 0 && not (isPrime x)) [1..10000]

-- 2 is not needed.
primes :: [Int]
primes = (filter (\x -> x `mod` 2 /= 0 && isPrime x) [1..10000])

primesUnderN :: Int -> [Int]
primesUnderN n = filter (\x -> x < n) primes

isGold :: Int -> Bool
isGold n = foldl (\a x -> a || (let b = sqrt(fromIntegral (n-x)/2) in fromIntegral(floor b) == b)) False $ primesUnderN n
