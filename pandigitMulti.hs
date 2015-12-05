-- problem 38
import Data.List (sort)

permute :: Eq a => [a] -> Int -> [[a]]
permute a 1 = map (\x -> [x]) a
permute a n = concat $ map (\x -> (map (\y -> x:y) (permute (filter (/=x) a) (n-1)))) a

multi :: String -> Int -> String
multi s n = show $ (read s) * n

multiHelper :: String -> String -> Int -> String
multiHelper a s n 
  | length a >= 9 = a
  | otherwise     = multiHelper (a++(multi s n)) s (n+1)

multiTo9Digit :: String -> String
multiTo9Digit s = multiHelper s s 2

isPandigit :: String -> Bool
isPandigit s = not $ snd $ foldl (\(a,b) x -> if b || x == '0' || x `elem` a then (a,True) else (x:a, False)) ([], False) s

createCandidate :: [String]
createCandidate = let a = "123456789"
                  in (permute a 1) ++
                     (permute a 2) ++
                     (permute a 3) ++
                     (permute a 4)

answer :: [String]
answer = let a = map multiTo9Digit createCandidate
             b = filter (\m -> (length m) == 9 && isPandigit m) a
         in sort b
