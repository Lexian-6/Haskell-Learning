doubleMe x = x + x

doubleSmall x = if x > 100
            then x
            else x*2

doubleSmall' x = succ (if x > 100 then x else x*2)

triangle = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2+b^2==c^2, a+b+c==24]