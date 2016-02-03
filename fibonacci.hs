fibnacci :: [Integer]

fibnacci = map fib [0..]

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fibnacci!!(n-1) + fibnacci!!(n-2)

-- get the index whose fibonacci numbers has 1000 digits.
-- head $ filter (\x -> (length $ show $ fib x) >=1000) [1..]
-- outputs : 4782
