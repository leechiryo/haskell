-- problem 43

permute :: Eq a => [a] -> Int -> [[a]]
permute a 1 = map (\x -> [x]) a
permute a n = concat $ map (\x -> (map (\y -> x:y) (permute (filter (/=x) a) (n-1)))) a

createCandidate :: [String]
createCandidate = filter (\x -> head x /= '0' && (x!!3) `elem` "02468" && (x!!5) `elem` "05") $ permute "0123456789" 10

answer :: Integer
answer = let a = createCandidate
             b = filter (\x -> (read (take 3 . drop 2 $ x)) `mod` 3 == 0) a
             c = filter (\x -> (read (take 3 . drop 4 $ x)) `mod` 7 == 0) b
             d = filter (\x -> (read (take 3 . drop 5 $ x)) `mod` 11 == 0) c
             e = filter (\x -> (read (take 3 . drop 6 $ x)) `mod` 13 == 0) d
             f = filter (\x -> (read (take 3 . drop 7 $ x)) `mod` 17 == 0) e
         in sum $ map read f
