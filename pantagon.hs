-- problem 44
import qualified Data.Map as Map

pantagon :: Int -> Int
pantagon n = n * (3*n -1) `div` 2

pantagons :: Int -> [Int]
pantagons max = map pantagon [1..max]

pantagonMap :: Map.Map Int Int
pantagonMap = Map.fromList $ zip (pantagons 10000) [1..10000]

findAnswer :: [(Int, Int)]
findAnswer = foldl (\a x -> let b = map (\s -> (s, x)) [1..x]
                                c = filter (\(l, r) -> Map.member ((pantagon l)+(pantagon r)) pantagonMap && Map.member ((pantagon r) - (pantagon l)) pantagonMap) b
                            in c ++ a
                   ) [] [1..10000]

-- It will take some time to get the answer which is [(1020,2167)]
-- so, pantagon 2167 - pantagon 1020 = 5482660
