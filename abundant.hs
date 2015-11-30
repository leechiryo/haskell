import Data.List
import qualified Data.Map as Map
import qualified Data.List as List

factors :: Int -> [Int]
factors n = foldl (\arr x->
                      if n `mod` x == 0 
                      then
                        if (n `div` x) /= x then (n `div` x):(x:arr) else x:arr
                      else arr) [] [1..floor $ sqrt $ fromIntegral n]

properFactors :: Int -> [Int]
properFactors n = filter (\x->x/=n) $ factors n

isAbundant :: Int -> Bool
isAbundant n = (sum $ properFactors n) > n

sumSelf :: [Int] -> Map.Map Int Int -> Map.Map Int Int
sumSelf [] sums = sums
sumSelf (x:ns) sums = List.foldl' (\m n ->if Map.member (x+n) m then m else Map.insert (x+n) (x+n) m) (sumSelf ns sums) (x:ns)

abundants :: [Int]
abundants = filter isAbundant [1..28123]

sumOf2Abundants :: Map.Map Int Int
sumOf2Abundants = sumSelf abundants Map.empty

numsCannotExpressedBy2AbundantsSum :: [Int]
numsCannotExpressedBy2AbundantsSum = filter (\x -> not $ Map.member x sumOf2Abundants) [1..28123]

answer :: Int
answer = sum numsCannotExpressedBy2AbundantsSum
