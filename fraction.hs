import Data.List

fraction :: Int -> Int -> [Int] -> [(Int, Int)]
fraction n d a = if n `mod` d == 0
                 then [(n `div` d, -1)]
                 else
                   if (n `mod` d) `elem` a
                   then [(n `div` d, unboxMaybe $ elemIndex (n `mod` d) a)]
                   else (n `div` d, -1) : (fraction ((n `mod` d)*10) d ((n `mod` d):a)) 

unboxMaybe :: Maybe Int -> Int
unboxMaybe Nothing = 0
unboxMaybe (Just n) = n

cyclesLen :: Int -> Int -> Int
cyclesLen a d = let frac = fraction a d []
                in (snd $ frac !! (length frac - 1)) + 1
