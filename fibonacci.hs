fibonacci :: Int -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = (fibonacci (n-1)) + (fibonacci (n-2))

fastFib :: Int -> Integer
fastFib 1 = 1
fastFib 2 = 1
fastFib n = snd $ foldl (\a _-> (snd a, (fst a) + (snd a))) (1, 1) [3..n]

-- get the index whose fibonacci numbers has 1000 digits.
-- head $ filter (\x -> (length $ show $ fastFib x) >=1000) [1..]
-- outputs : 4782
