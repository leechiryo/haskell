primeFactors :: (Integral a) => a -> [a]
primeFactors n = if filter (\x -> n `mod` x == 0) [1 .. floor $ sqrt n'] == [1] 
                 then [1, n]
                 else foldr (++) [] $ map primeFactors $ filter (\x -> n `mod` x == 0) [1 .. floor $ sqrt n']
                 where n' = fromIntegral n

factorCount :: Integer -> Int
factorCount n = (*2) $ length $ filter (\x -> n `mod` x == 0) [1 .. floor $ sqrt n']
                where n' = fromIntegral n
