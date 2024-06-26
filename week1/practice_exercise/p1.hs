module Practice1 where

{-
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs
-}
a::[Int]
a=[1,2,3]

myBinop :: (Int -> Int -> Int) -> Int -> ([Int] -> Int)
myBinop f z [] = z
myBinop f z (x:xs) = f x (myBinop f z xs)

mySum :: [Int] -> Int
myProduct :: [Int] -> Int
mySum = foldr (+) 0
myProduct = foldr (*) 1

myMap :: ( a -> b ) -> [a] -> [b]
myMap f = foldr (\x -> (f x :)) []

{-
Define foldl with foldr
-}

-- foldl' = foldr (\x -> (f x :)) []

length' xs = sum [ 1 | _ <- xs]

removeNonUppercase xs = [ x | x <- xs, x `elem` ['A' .. 'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
[ [ x | x <- xs, odd x] | xs <- xxs ]