delChar :: Char -> [Char] -> [Char]
delChar c cs = filter (/=c) cs

addChar :: Char -> [[Char]] -> [[Char]]
addChar c css = map (c:) css

lexicographic :: [Char] -> [[Char]]
lexicographic (c:[]) = [[c]]
lexicographic cs = foldl (\a x -> foldr (:) (addChar x (lexicographic $ delChar x cs)) a) [] cs

-- output: lexicographic "012"
--         ["012","021","102","120","201","210"]
--         lexicographic "0123456789" !! 999999
--         2783915460
-- is the millionth lexicographic permutation
