import Data.Char as C

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChains2 :: Int
numLongChains2 = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

fromHex :: String -> Int
fromHex s = foldl1 (\x y -> 16*x + y) $ map C.digitToInt s
