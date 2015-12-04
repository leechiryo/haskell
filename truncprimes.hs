import Data.List

divisors :: Int -> [Int]
divisors n = let cs = [1..floor $ sqrt (fromIntegral n :: Double)]
             in (map head . group . sort) $ concat $ map (\x -> if n `mod` x == 0 then [x, n `div` x] else []) cs

isPrime :: Int -> Bool
isPrime n = (length $ divisors n) == 2

permute :: [a] -> Int -> [[a]]
permute a 1 = map (\x -> [x]) a
permute a n = concat $ map (\x -> (map (\y -> x:y) (permute a (n-1)))) a

createCandidate :: [String]
createCandidate = let a = "123579"
                  in (filter (\ns -> let c = ns!!((length ns) -1) in c /= '9') $ filter (\(n:ns) -> n /= '9') $ filter (\(n:ns) -> not ('2' `elem` ns) && not ('5' `elem` ns)) $ permute a 2) ++
                     (filter (\ns -> let c = ns!!((length ns) -1) in c /= '9') $ filter (\(n:ns) -> n /= '9') $ filter (\(n:ns) -> not ('2' `elem` ns) && not ('5' `elem` ns)) $ permute a 3) ++
                     (filter (\ns -> let c = ns!!((length ns) -1) in c /= '9') $ filter (\(n:ns) -> n /= '9') $ filter (\(n:ns) -> not ('2' `elem` ns) && not ('5' `elem` ns)) $ permute a 4) ++
                     (filter (\ns -> let c = ns!!((length ns) -1) in c /= '9') $ filter (\(n:ns) -> n /= '9') $ filter (\(n:ns) -> not ('2' `elem` ns) && not ('5' `elem` ns)) $ permute a 5) ++
                     (filter (\ns -> let c = ns!!((length ns) -1) in c /= '9') $ filter (\(n:ns) -> n /= '9') $ filter (\(n:ns) -> not ('2' `elem` ns) && not ('5' `elem` ns)) $ permute a 6)

isTruncablePrimeR :: String -> Bool
isTruncablePrimeR (c:[]) = if c `elem` "2357" then True else False
isTruncablePrimeR s = (isPrime $ read s) && (isTruncablePrimeR $ tail s)

isTruncablePrimeL :: String -> Bool
isTruncablePrimeL (c:[]) = if c `elem` "2357" then True else False
isTruncablePrimeL s = (isPrime $ read s) && (isTruncablePrimeL $ init s)

isTruncablePrime :: String -> Bool
isTruncablePrime s = (isTruncablePrimeR s) && (isTruncablePrimeL s)

answer :: Int
answer = sum $ map read $ filter isTruncablePrime createCandidate
