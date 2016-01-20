-- problem 47
import Data.List
import qualified Data.Map as Map

divisors :: Int -> [Int]
divisors n = let cs = [1..floor $ sqrt (fromIntegral n :: Double)]
             in concat $ map (\x -> if n `mod` x == 0 then [x, n `div` x] else []) cs

mergeResult :: [([Int], Map.Map Int [Int])] -> ([Int], Map.Map Int [Int])
mergeResult a = foldl (\(accx, accy) (x, y) -> (x ++ accx, Map.union y accy)) ([], Map.empty) a

primeFactors' :: (Int, Map.Map Int [Int]) -> ([Int], Map.Map Int [Int])
primeFactors' (n, m) = if Map.member n m 
                       then (Map.findWithDefault [] n m, m)
                       else if divisors n == [1, n] 
                            then ([n], Map.insert n [n] m)
                            else let a = map (\f -> primeFactors' (f, m)) $ filter (\x -> x /= 1 && x/=n) $ divisors n
                                 in mergeResult a
                            where n' = fromIntegral n

primeFactors :: (Int, Map.Map Int [Int]) -> ([Int], Map.Map Int [Int])
primeFactors (n, m) = let (n1, m1) = primeFactors' (n, m)
                          n2 = (map head . group . sort) n1
                      in (n2, Map.insert n n2 $ Map.union m m1)

primeCount :: Int -> Int
primeCount n = length $ primeFactors (n, Map.empty)
