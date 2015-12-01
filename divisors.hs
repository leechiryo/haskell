import Data.List
divisors :: Integer -> [Integer]
divisors n = let cs = [1..floor $ sqrt (fromIntegral n :: Double)]
             in (map head . group . sort) $ concat $ map (\x -> if n `mod` x == 0 then [x, n `div` x] else []) cs

properDivisors :: Integer -> [Integer]
properDivisors n = filter (/=n) $ divisors n

amicable :: Integer -> Bool
amicable n = let c = (sum $ divisors n) - n
                 s = (sum $ divisors c) - c
             in if n == s && c /= n then True else False

isPrime :: Integer -> Bool
isPrime n = (length $ divisors n) == 2

quadratics :: Integer -> Integer -> Integer -> Integer 
quadratics a b x = x^2 + a*x + b
