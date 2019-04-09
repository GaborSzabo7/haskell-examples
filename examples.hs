-- Types and Typelasses
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

-- Integer is not bounded, it can be used to represent really really big numbers
factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * r * pi

-- Type variable, pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky _ = "Sorry, you are out of luck."

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe _ = "Any number"

factorial2 :: (Integral a) => a -> a
factorial2 0 = 1
factorial2 1 = 1
factorial2 n = n * factorial2 (n-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors2 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors2 (a, b) (c, d) = (a + c, b + d)

third :: (a ,b, c) -> c
third (_, _, x) = x

head2 :: [a] -> a
head2 [] = error "Can't call head on an empty list."
head2 (x:_) = x

tell :: (Show a) => [a] -> String
tell []         = "Empty list."
tell (x:[])     = "The list has one element: " ++ show x
tell (x:y:[])   = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_)    = "The list is long."

length2 :: (Num b) => [a] -> b
length2 []      = 0
length2 (_:xs)  = 1 + length2 xs

sum2 :: (Num a) => [a] -> a
sum2 []     = 0
sum2 (x:xs) = x + sum2 xs

-- as patterns
capital :: String -> String
capital []          = "Empty string."
capital all@(x:xs)  = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You are underweight."
    | bmi <= 25.0 = "You are supposedly normal."
    | bmi <= 30.0 = "You are fat."
    | otherwise   = "You are a whale."

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | weight / height^2 <= 18.5 = "You are underweight."
    | weight / height^2 <= 25.0 = "You are supposedly normal."
    | weight / height^2 <= 30.0 = "You are fat."
    | otherwise   = "You are a whale."

-- where bindings aren't shared across function bodies of different patterns
bmiTell3 :: (Show a, RealFloat a) => a -> a -> String
bmiTell3 weight height
        | value <= 18.5 = "You are underweight. The value is " ++ show value
        | value <= 25.0 = "You are supposedly normal. The value is " ++ show value
        | value <= 30.0 = "You are fat. The value is " ++ show value
        | otherwise     = "You are a whale."
    where value = weight / height^2

max2 :: (Ord a) => a -> a -> a
max2 a b | a > b = a | otherwise = b

compare2 :: (Ord a) => a -> a -> Ordering
compare2 a b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT 

bmiTell4 :: (Show a, RealFloat a) => a -> a -> String
bmiTell4 weight height
        | value <= skinny = "You are underweight. The value is " ++ show value
        | value <= normal = "You are supposedly normal. The value is " ++ show value
        | value <= fat    = "You are fat. The value is " ++ show value
        | otherwise       = "You are a whale."
    where value     = weight / height^2
          skinny    = 18.5
          normal    = 25.0
          fat       = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ " " ++ [l]
    where (f:_) = firstname
          (l:_) = lastname

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * r * pi * h
        topArea  = r^2 * pi
    in sideArea + 2 * topArea

head3 :: [a] -> a
head3 xs = case xs of []    -> error "No head for empty list."
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []   -> "an empty list."
                                               [x]  -> "a singleton list."
                                               xs   -> "a long list."

describeList2 :: [a] -> String
describeList2 xs = "The list is " ++ what xs
    where what []   = "an empty list."
          what [x]  = "a singleton list."
          what xs   = "a long list."

-- Recursion
maximum2 :: (Ord a) => [a] -> a
maximum2 []     = error "There is no maximum of an empty list."
maximum2 [x]    = x
maximum2 (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = maximum2 xs

maximum3 :: (Ord a) => [a] -> a
maximum3 []     = error "There is no maximum of an empty list."
maximum3 [x]    = x
maximum3 (x:xs) = max x (maximum3 xs)

replicate2 :: (Num i, Ord i) => i -> a -> [a]
replicate2 n x
    | n <= 0    = []
    | otherwise = x : replicate2 (n-1) x

take2 :: (Num i, Ord i) => i -> [a] -> [a]
take2 n _
    | n <= 0    = []
take2 _ []      = []
take2 n (x:xs)  = x : take2 (n-1) xs

reverse2 :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse2 xs ++ [x]

repeat2 :: a -> [a]
repeat2 x = x : repeat2 x

zip2 :: [a] -> [b] -> [(a,b)]
zip2 _ []           = []
zip2 [] _           = []
zip2 (x:xs) (y:ys)  = (x,y) : zip2 xs ys

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 a [] = False
elem2 a (x:xs)
    | a == x    = True
    | otherwise = elem2 a xs

elem3 :: (Eq a) => a -> [a] -> Bool
elem3 a []      = False
elem3 a (x:xs)  = if a == x then True else elem3 a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

mulThree :: (Num a) => a -> a -> a -> a
mulThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare x 100

dividedByTen :: (Floating a) => a -> a
dividedByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith2 _ [] _             = []
zipWith2 _ _ []             = []
zipWith2 f (x:xs) (y:ys)    = f x y : zipWith2 f xs ys

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f = g 
    where g x y = f y x

flip3 :: (a -> b -> c) -> b -> a -> c
flip3 f y x = f x y

-- Maps and filters
quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (x:xs) =
    let smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted  = quicksort (filter (> x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = mod x 3892 == 0

chain  :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (div n 2)
    | odd n  = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

sum3 :: (Num a) => [a] -> a
sum3 xs = foldl (\acc x -> acc + x ) 0 xs

sum4 :: (Num a) => [a] -> a
sum4 = foldl (+) 0

elem4 :: (Eq a) => a -> [a] -> Bool
elem4 y ys = foldl (\acc x -> if x == y then True else acc) False ys

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acc -> f x : acc) [] xs

map3 :: (a -> b) -> [a] -> [b]
map3 f xs = foldl (\acc x -> acc ++ [f x]) [] xs

sum5 :: (Num a) => [a] -> a
sum5 = foldl1 (+)
