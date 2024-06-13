head' :: [a] -> a
head' [] = error "Not good"
head' (x:xs) = x

tell :: (Show a) => [a] -> String
tell [] = "An empty list!"
tell (x:[]) = "A list with one argument " ++ (show x)
tell x = "List with more than one arguments!"

-- list comprehension and recursion for length function

-- list comprehension
length' :: Integral b => [a] -> b
length' xs = sum [ 1 | x <- xs]

-- Recursion
length'' :: Integral b => [a] -> b
length'' [] = 0
length'' (_:xs) = 1+ length'' xs

-- Similarly for sum
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x+sum' xs

-- As pattern

capital :: [Char] -> [Char]
capital [] = "Oops, an empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++  [x]

-- If Else statements and Guards

bmiTell :: Float -> String
bmiTell bmi = 
    if bmi <= 18.5 then "underweight"
    else if bmi <= 25.0 then "normal"
    else if bmi <= 30.0 then "overweight"
    else "WTF"

bmiTell' :: Float -> String
bmiTell' bmi
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "overweight"
    | otherwise = "WTF"

bmiTell'' :: Float -> Float -> String
bmiTell'' weight height
    | weight / height ^2 <= 18.5 = "underweight with a bmi value of " ++ show (weight / height ^2)
    | weight / height ^2 <= 25.0 = "normal with a bmi value of " ++ show (weight / height ^2)
    | weight / height ^2 <= 30.0 = "overweight with a bmi value of " ++ show (weight / height ^2)
    | otherwise = "WTF with a bmi value of " ++ show (weight / height ^2)

-- Can be compared means have ordering
max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b 
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a < b = LT
    | a == b = EQ
    | a > b = GT

-- Rewrite bmiTell with where clause and pattern matching
bmiTell''' :: Float -> Float -> String
bmiTell''' weight height
    | bmi <= skinny = "underweight with a bmi value of " ++ show (bmi)
    | bmi <= normal = "normal with a bmi value of " ++ show (bmi)
    | bmi <= fat = "overweight with a bmi value of " ++ show (bmi)
    | otherwise = "WTF with a bmi value of " ++ show (bmi)
    where   bmi=weight / height ^2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstname
            (l:_) = lastname

calcBmis:: RealFloat a => [(a, a)] -> [a]
calcBmis [] = []
calcBmis (x:xs) = bmi (fst x) (snd x): calcBmis xs
    where   bmi weight height = weight / height ^2

calcBmis':: RealFloat a => [(a, a)] -> [a]
calcBmis' xs = [ bmi weight height | (weight, height) <- xs]
    where   bmi weight height = weight / height ^ 2

-- What about let
cylinder :: Float -> Float -> Float
cylinder r h = 
    let top_area = pi * r^2
        side_area = 2*pi*r*h
    in 2*top_area + side_area

sum_3 = (let (a,b,c)=(1,2,3) in a+b+c)*100

calcBmis'':: RealFloat a => [(a, a)] -> [a]
calcBmis'' xs = [ let bmi x y = x / y^2 in bmi weight height | (weight, height) <- xs]

calcBmis''' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis''' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- visible to the predicates after let
calcBmis'''' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis'''' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- We can also use let in, but the scope would be restricted in that prediate
calcBmis''''' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis''''' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2 in bmi >= 25.0, let bmi = w / h ^ 2]