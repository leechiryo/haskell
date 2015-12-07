-- problem 42
import qualified Data.Map as Map
import qualified Data.List as List
import System.IO

unboxMaybe :: Maybe Int-> Int
unboxMaybe Nothing = 0
unboxMaybe (Just n) = n

nameScores :: String -> Int
nameScores s = let scoreMap = Map.fromList $ zip ['A'..'Z'] [1..26]
               in sum $ map (\c -> unboxMaybe $ Map.lookup c scoreMap) s

nameValue :: (Int, String) -> Int
nameValue (n, s) = n * (nameScores s)

sumNameValues :: [(Int, String)] -> Int
sumNameValues ps = sum $ map nameValue ps

split :: [String] -> Char -> [String]
split [] c = if any (==c) ['A'..'Z'] then [[c]] else []
split ([]:a) c = if any (==c) ['A'..'Z'] then ([c]:a) else ([]:a)
split (s:a) c = if any (==c) ['A'..'Z'] then (s ++ [c]):a else []:(s:a)

triangleVals :: Map.Map Int Int
triangleVals = let a = [1..100]
               in Map.fromList $ zip (map (\x-> x * (x+1) `div` 2) a) a

main = do
       handle <- openFile "p042_words.txt" ReadMode
       contents <- hGetContents handle
       let ([] : names) = foldl split [] contents
       putStr $ show $ length $ filter (==True) $ map (\x -> Map.member (nameScores x) triangleVals) names
