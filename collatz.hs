collatz :: Integer -> [Integer] -> Integer
collatz 1 es = 1
collatz n es = if length es >= n
               then es !! (length - n)
               else if n `mod` 2 == 0
                    then (+1) $ collatz (n `div` 2) es
                    else (+1) $ collatz (n*3+1) es
