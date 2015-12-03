import Data.List

permute :: [Int] -> Int -> [[Int]]
permute a 1 = map (\x -> [x]) a
permute a n = concat $ map (\x -> (map (\y -> x:y) (permute (filter (/=x) a) (n-1)))) a

panparams :: [Int] -> Int -> (Int, Int)
panparams [] _ = (0,0)
panparams (m:ms) n = let p = panparams ms (n-1)
                     in if n>=0 
                        then ((m*10^n) + (fst p), snd p)
                        else (fst p, m * 10 ^ (length ms) + snd p)

panmulti :: (Int, Int) -> [Int]
panmulti p = let m = (fst p) * (snd p)
               in map (\x -> read (x:[])) $ show m

ispan :: [Int] -> [Int] -> Bool
ispan a b = sort (a ++ b) == [1..9]

pandigit :: [(Int, Int, Int)]
pandigit = let a = permute [1..9] 5
               b = map (\x -> let p = panparams x 0
                              in (fst p, snd p, ispan x $ panmulti p)) a
               c = map (\x -> let p = panparams x 1
                              in (fst p, snd p, ispan x $ panmulti p)) a
               d = (filter (\(_,_,x) -> x) b) ++ (filter (\(_,_,x) -> x) c) 
            in map (\(x,y,_) -> (x, y, x*y)) d

-- problem 32 answer
-- sum $ map head $ group $ sort $ map (\(_,_,m) -> m) pandigit
-- output: 45228
