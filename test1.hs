take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n [] = []
take' n (x:xs)
  | n <= 0    = []
  | otherwise = x:take' (n-1) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs
