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

maxConsecPrimes :: Integer -> Integer -> Integer
maxConsecPrimes a b = fst $ foldl (\a x -> 
                                   if (snd a) 
                                   then a 
                                   else 
                                     if isPrime x 
                                     then ((fst a) + 1, False) 
                                     else (fst a, True)
                                  ) (0, False) $ map (quadratics a b) [0..1000]

-- find the max consecutive primes
-- let b = reverse $ filter isPrime [1..1000]
-- let a = filter (\x -> x `mod` 2 /= 0) [-1000..1000]
-- map (\y -> maximum $ map (\x -> maxConsecPrimes x y) a) b
-- get the maximum consecutive primes count is 71 when the number of b is 971
-- let i = Data.List.elemIndex 71 $ map (\x -> maxConsecPrimes x 971) a
-- a !! i
-- get the maximum consecutive primes count is 71 when the number of a is -61
-- so, a * b = 971 * (-61) = -59231
