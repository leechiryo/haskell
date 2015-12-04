import Numeric (showIntAtBase, readInt)
import Data.Char (intToDigit, digitToInt)

permute :: [a] -> Int -> [[a]]
permute a 1 = map (\x -> [x]) a
permute a n = concat $ map (\x -> (map (\y -> x:y) (permute a (n-1)))) a

makePanlidrome :: [a] -> [a]
makePanlidrome s = (reverse $ tail s) ++ s

makePanlidrome2 :: [a] -> [a]
makePanlidrome2 s = (reverse s) ++ s

createCandidate :: [Int]
createCandidate = let seed1 = permute "0123456789" 1
                      seed2 = permute "0123456789" 2
                      seed3 = permute "0123456789" 3
                      pan1 = filter (\x->head x /= '0') seed1
                      pan2 = filter (\x->head x /= '0') $ map makePanlidrome2 seed1
                      pan3 = filter (\x->head x /= '0') $ map makePanlidrome seed2
                      pan4 = filter (\x->head x /= '0') $ map makePanlidrome2 seed2
                      pan5 = filter (\x->head x /= '0') $ map makePanlidrome seed3
                      pan6 = filter (\x->head x /= '0') $ map makePanlidrome2 seed3
                  in map read $ pan1 ++ pan2 ++ pan3 ++ pan4 ++ pan5 ++ pan6

showIntInBin :: Int -> String
showIntInBin n = showIntAtBase 2 intToDigit n ""

readIntFromBin :: String -> Int
readIntFromBin s = fst $ head $ readInt 2 (`elem` "01") digitToInt s

isPanlidrome :: Eq a => [a] -> Bool
isPanlidrome ns = reverse ns == ns

answer :: [Int]
answer =  map readIntFromBin $ filter isPanlidrome $ map showIntInBin createCandidate
