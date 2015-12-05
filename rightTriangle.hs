-- problem 39
import qualified Data.Map.Strict as Map
import qualified Data.List as List

intSqrt :: Int -> Int
intSqrt n = let a = floor $ sqrt (fromIntegral n)
            in if a^2 == n then a
            else 0

createCandidate :: [(Int, Int, Int)]
createCandidate = let a = [(x,y) | x<-[1..499], y<-[1..x]]
                  in filter (\(x,y,z) -> z /= 0 && x+y+z<=1000) $ map (\(x,y) -> (x,y, intSqrt (x^2 + y^2))) a

answer :: [(Int, Int)]
answer = List.sortOn snd $ Map.toList $ foldl (\a (x,y,z) -> if Map.member (x+y+z) a then Map.adjust (\c->c+1) (x+y+z) a else Map.insert (x+y+z) 1 a) Map.empty createCandidate

-- answer output an array, the last element is the answer, which is (840,8), means that 840 has 8 solutions.
-- filter (\(x,y,z) -> x+y+z==840) createCandidate
-- will output the eight solutions.
-- output : [(252,240,348),(280,210,350),(315,168,357),(336,140,364),(350,120,370),(360,105,375),(390,56,394),(399,40,401)]
