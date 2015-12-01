fraction :: Int -> Int -> [Int]
fraction n d = if n `mod` d == 0
               then [n `div` d]
               else (n `div` d) : (fraction ((n `mod` d)*10) d) 
