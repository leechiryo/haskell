import Data.List

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
