import qualified Data.Map as Map

fact :: Int -> Int
fact 0=1
fact 1=1
fact 2=2
fact 3=6
fact 4=24
fact 5=120
fact 6=720
fact 7=5040
fact 8=40320
fact 9=362880

digitFactSum :: Int -> Int
digitFactSum n = sum $ map (\x -> fact $ read (x:[])) $ show n

-- filter (\x -> digitFactSum x == x) [1..2540160]
-- output: [1,2,145,40585]
