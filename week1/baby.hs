doubleMe x = x + x

doubleSmall x = if x > 100
            then x
            else x*2

doubleSmall' x = succ (if x > 100 then x else x*2)