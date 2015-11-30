weekday :: [Integer] -> (Integer, Integer) -> [Integer]
weekday [] (1901, 1) = [2] -- 1901/1/1 is Tuesday
weekday (w:ws) (y, m) = if any (==(m-1)) [4,6,9,11] -- small month
                        then ((w+2) `mod` 7):(w:ws) -- 0 is sunday, 30 `mod` 7 = 2
                        else
                          if any (==(m-1)) [0, 1, 3, 5, 7, 8, 10] -- big month, 0 is December
                          then ((w+3) `mod` 7):(w:ws) -- 0 is sunday, 31 `mod` 7 = 3
                          else
                            if y `mod` 4 == 0 -- February, leap year
                            then ((w+1) `mod` 7):(w:ws) -- 0 is sunday, 29 `mod` 7 = 1
                            else w:(w:ws) -- 0 is sunday, 28 `mod` 7 = 0

sundaycount :: [(Integer, Integer)] -> Int
sundaycount yms = length $ filter (==0) $ foldl weekday [] yms
