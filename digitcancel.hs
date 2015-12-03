hasSameChars :: ([Char], [Char]) -> Bool
hasSameChars (a, b) = (length $ filter (\x -> x `elem` b) a) > 0

delOneChar :: [Char] -> Char -> [Char]
delOneChar s c = fst $ foldr (\x (ss, b) -> if not b && x == c then (ss, True) else (x:ss, b)) ([], False) s

delSameChar :: ([Char], [Char]) -> ([Char], [Char])
delSameChar (a,b) = foldl (\(aa, ab) x -> if x `elem` ab then (delOneChar aa x, delOneChar ab x) else (aa,ab)) (a,b) a

calFraction :: ([Char], [Char]) -> Double
calFraction (a,b) = (read a) / (read b)

digitCancellable :: ([Char], [Char]) -> Bool
digitCancellable a = hasSameChars a 
                     && calFraction a < 1 
                     && (length $ fst $ delSameChar a) > 0
                     && (length $ snd $ delSameChar a) > 0
                     && not  ('0' `elem` (fst a))
                     && calFraction a == calFraction (delSameChar a)

-- create the candidate set
-- let a = [(show a, show b) | a<-[10..99], b<-[10..99]]
-- filter the result
-- filter digitCancellable a
-- output: [("16","64"),("19","95"),("26","65"),("49","98")]
-- calcute the fraction: 16/64 = 1/4, 19/95 = 1/5, 26/65 = 2/5, 49/98 = 1/2
-- product them up, got 1/100
