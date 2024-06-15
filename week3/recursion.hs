maximum' :: Ord a => [a] -> a
maximum' [] = error "Empty list error"
maximum' [x] = x
maximum' (x:xs)
    |   x > rest = x
    |   otherwise = rest
    where   rest = maximum' xs

replicate' :: Integral l => l -> a -> [a]
replicate' 0 _ = []
replicate' x y
    |   x > 0 = y:(replicate' (x-1) y)
    |   otherwise = error "Replicate can't take negative number"

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    |   a == x = True
    |   otherwise = elem' a xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort first_halve ++ [x] ++ quicksort second_halve
    where   first_halve = [ a | a <- xs, a <= x]
            second_halve = [ a | a <- xs, a > x]
