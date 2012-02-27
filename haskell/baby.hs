doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

a =  "hello" ++ " " ++ "world"
b = "b " ++ a

c = [x*2 | x <- [1..10]]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

--[fst t + snd t | t <- zip [1,2,3,4,5] [6,7,8,9,10]]

--zip [1..] ["apple", "orange", "cherry", "mango"]



triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]


factorial :: Integer -> Integer
factorial n = product [1..n]


--(read "5" :: Float) * 4
--read "[1,2,3,4]" :: [Int]
--read "(3, 'a')" :: (Int, Char)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName _ = "Unknown !"

charNameValue = take 5 (map charName ['a'..])

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

--xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
--[a+b | (a,b) <- xs]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

-- initials :: String -> String -> String
-- initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
--     where (f:_) = firstname
--           (l:_) = lastname

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

aSdf = (let a = 100; b = 200; c = 300; in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

-- calcBmis :: (RealFloat a) => [(a, a)] -> [a]
-- calcBmis xs = [bmi w h | (w, h) <- xs]
--     where bmi weight height = weight / height ^ 2

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
          let low = quicksort [a | a <- xs, a <= x]
              high = quicksort [a | a <- xs, a > x]
              in low ++ [x] ++ high

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = map f x y : zipWith' f xs ys

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
