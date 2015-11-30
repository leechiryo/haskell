import qualified Data.Map as Map
import qualified Data.List as List
sumSelf :: [Int] -> Map.Map Int Int -> Map.Map Int Int
sumSelf [] sums = sums
sumSelf (x:ns) sums = List.foldl' (\m n ->if Map.member (x+n) m then m else Map.insert (x+n) (x+n) m) (sumSelf ns sums) (x:ns)
