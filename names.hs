import qualified Data.Map as Map
import qualified Data.List as List
import System.IO

unboxMaybe :: Maybe Integer -> Integer
unboxMaybe Nothing = 0
unboxMaybe (Just n) = n

nameScores :: String -> Integer
nameScores s = let scoreMap = Map.fromList $ zip ['A'..'Z'] [1..26]
               in sum $ map (\c -> unboxMaybe $ Map.lookup c scoreMap) s

nameValue :: (Integer, String) -> Integer
nameValue (n, s) = n * (nameScores s)

sumNameValues :: [(Integer, String)] -> Integer
sumNameValues ps = sum $ map nameValue ps

split :: [String] -> Char -> [String]
split [] c = if any (==c) ['A'..'Z'] then [[c]] else []
split ([]:a) c = if any (==c) ['A'..'Z'] then ([c]:a) else ([]:a)
split (s:a) c = if any (==c) ['A'..'Z'] then (s ++ [c]):a else []:(s:a)

main = do
       handle <- openFile "p022_names.txt" ReadMode
       contents <- hGetContents handle
       let ([] : names) = foldl split [] contents

       putStr $ show $ zip [1 .. fromIntegral(length names)] $ List.sort names
       --putStr $ show names
       putStr $ show $ sumNameValues $ zip [1 .. fromIntegral(length names)] $ List.sort names

       hClose handle
