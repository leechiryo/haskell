substring :: [a] -> Int -> Int -> [a]
substring s st n = map (\x -> s !! x) [st..(st+n-1)]

productstr :: [Char] -> Integer
productstr s = let darr = map (read . (\c -> [c])) s :: [Integer]
               in foldl (*) 1 darr
