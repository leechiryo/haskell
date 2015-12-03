import Data.List

divisors :: Int -> [Int]
divisors n = let cs = [1..floor $ sqrt (fromIntegral n :: Double)]
             in (map head . group . sort) $ concat $ map (\x -> if n `mod` x == 0 then [x, n `div` x] else []) cs

isPrime :: Int -> Bool
isPrime n = (length $ divisors n) == 2

permute :: [a] -> Int -> [[a]]
permute a 1 = map (\x -> [x]) a
permute a n = concat $ map (\x -> (map (\y -> x:y) (permute a (n-1)))) a

createCandidate :: [Int]
createCandidate = let a = "13579"
                  in [2, 3, 5, 7] ++
                     (map read $ permute a 2) ++
                     (map read $ permute a 3) ++
                     (map read $ permute a 4) ++
                     (map read $ permute a 5) ++
                     (map read $ permute a 6)

shift :: Int -> Int
shift n = let c:cs = show n
          in read $ cs ++ (c:[])

-- solve the problem 35
-- let a = createCandidate
-- let b = filter isPrime a
-- let c = filter isPrime $ map shift b
-- let d = filter isPrime $ map shift c
-- let e = filter isPrime $ map shift d
-- let f = filter isPrime $ map shift e
-- let g = filter isPrime $ map shift f
-- let h = filter isPrime $ map shift g
-- length h
-- output: 55 will be stable
