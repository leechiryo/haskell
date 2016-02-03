data Levenshtein = Levenshtein [[(Int,Char,Int,Int)]]

(!) :: Levenshtein -> (Int, Int) -> Int
(!) (Levenshtein nss) (r, c) = let (x,_,_,_) = (nss!!r)!!c in x

instance Show Levenshtein where
  show (Levenshtein []) = ""
  show (Levenshtein (ns:nss)) = (show ns) ++ "\n" ++ show (Levenshtein nss)

distMatrix :: String -> String -> Levenshtein
distMatrix s1 s2 = table
                   where
                   l1 = length s1
                   l2 = length s2
                   table :: Levenshtein
                   table = Levenshtein [[dist x y | y <- [0..l2]] | x<- [0..l1]]
                   dist 0 j = (j, 'z', 0, 0)
                   dist i 0 = (i, 'z', 0, 0)
                   dist i j = let dd = (table!(i-1,j))+1 
                                  di = (table!(i,j-1))+1 
                                  dr = if s1!!(i-1) == s2!!(j-1) then table!(i-1,j-1) else (table!(i-1, j-1))+1
                                  d = minimum [dd,di,dr]
                              in if dd == d then 
                                   (dd, 'd', i-1, j)
                                 else if di == d then
                                   (di, 'i', i, j-1)
                                 else if dr == d && s1!!(i-1) == s2!!(j-1) then
                                   (dr, 'c', i-1, j-1)
                                 else
                                   (dr, 'r', i-1, j-1)
