-- problem 47
import Data.List

divisors :: Int -> [Int]
divisors n = let cs = [1..floor $ sqrt (fromIntegral n :: Double)]
             in concat $ map (\x -> if n `mod` x == 0 then [x, n `div` x] else []) cs

primeFactors' :: Int -> [Int]
primeFactors' n = if divisors n == [1, n] 
                 then [n]
                 else concat $ map primeFactors $ filter (\x -> x /= 1 && x/=n) $ divisors n
                 where n' = fromIntegral n

primeFactors :: Int -> [Int]
primeFactors n = (map head . group . sort) $ primeFactors' n

primeCount :: Int -> Int
primeCount n = length $ primeFactors n
