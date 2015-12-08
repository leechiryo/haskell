-- problem 45
--
-- Triangle T(n) = n*(n+1) / 2,
-- => if n*(n+1)/2 = x
-- => then n^2 + n -2x = 0, where a = 1, b = 1, c = -2x
-- => n = [-1 (+/-) sqrt(1+8x)] / 2
--
-- Pentagonal P(n) = n*(3*n-1) / 2,
-- => if n*(3*n-1)/2 = x
-- => then 3*n^2 -n -2x = 0, where a = 3, b = -1, c = -2x
-- => n = [1 (+/-) sqrt(1+24*x)]/6

-- Example: x = 40775
-- so, Triangle n = 285
--     Pentagonal n = 165
-- 
-- The condition that n is an integer should be:
-- 1. sqrt(1+8x) is an integer and the value is odd.
-- 2. sqrt(1+24x) is an integer and the value plus one can
--    be devided by 6.

hexagonal :: Integer -> Integer
hexagonal n = 2 * n^2 - n

hexagonals :: [Integer]
hexagonals = map hexagonal [1..100000]

answer :: [Integer]
answer = filter (\x -> let a = sqrt(fromIntegral (1 + 8*x):: Double)
                           b = sqrt(fromIntegral (1 + 24*x)::Double)
                           c = floor a
                           d = floor b
                       in fromIntegral c == a && fromIntegral d == b && c `mod` 2 /= 0 && (d+1) `mod` 6 == 0) hexagonals
