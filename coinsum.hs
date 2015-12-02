--In England the currency is made up of pound, £, and pence, p, 
--and there are eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:
-- 
-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
-- How many different ways can £2 be made using any number of coins?
-- 1p[0..200]
-- 2p[0..100]
-- 5p[0..40]
-- 10p[0..20]
-- 20p[0..10]
-- 50p[0..4]
-- 1 pound [0..2]
-- 2 pound [0..1]

coinsum :: [Int] -> Int -> Int
coinsum [p] t = if t `mod` p == 0 then 1 else 0
coinsum (p:ps) t = foldl (\a x -> a + coinsum ps (t - p * x)) (t `div` p) [0 .. (t `div` p)]
