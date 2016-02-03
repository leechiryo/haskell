fibnacci :: [Integer]

fibnacci = map fib [0..]

fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fibnacci!!(n-1) + fibnacci!!(n-2)
