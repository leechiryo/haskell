-- problem 40

getLength :: Int -> Int
getLength n = let a = zip [1..n] $ map (\x->9*10^x) [0..n-1]
              in foldl (\a (n,c) -> a + n*c) 0 a

lengthMap :: [(Int, Int)]
lengthMap = map (\x -> (x, getLength x)) [1..6]

biggestLowerLength :: Int -> (Int, Int)
biggestLowerLength n = foldl (\(ab, al) (b,l) -> if l > n then (ab,al) else (b,l)) (0,0) lengthMap

champerbit :: Int -> Int
champerbit n = let (a, b) = biggestLowerLength n
                   c = a + 1
                   d = (n - b) `div` c
                   e = (n - b) `mod` c
               in read $ ((show (10 ^ a + d)) !! e):[]

answer :: [Int]
answer = map champerbit [0, 9, 99, 999, 9999, 99999, 999999]

-- To get the answer is 210
-- use the following
-- product answer
-- output: 210
