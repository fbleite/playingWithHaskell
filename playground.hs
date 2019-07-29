doubleMe x = x + x  

doubleThis x = x + x  


tlvParser :: [Char] -> ([Char], [Char], [Char])
tlvParser (t1:t2:l1:l2:v) = (t1:t2:[], l1:l2:[], v)


bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"


describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
