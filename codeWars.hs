-- module SquareDigit where
import Data.List (delete)
    


squareDigit :: Int -> Int
squareDigit num = read (squareDigitIn (show (num)))::Int


squareDigitIn :: [Char] -> [Char]
squareDigitIn [] = []
squareDigitIn (x:xs) = show((read(x:[])::Int) ^ 2 ) ++ squareDigitIn xs


removeSmallest :: [Int] -> [Int]
removeSmallest [] = []
removeSmallest (x:xs)
    | x == min = xs
    | otherwise = x:removeSmallest xs
    where min = minimum (x:xs)

removeSmallest2 :: Ord a => [a] -> [a]
removeSmallest2 xs = delete (minimum xs) xs
    -- removeSmallest xs =
    --     let min = minimum xs
    --         p x = min /= x
    --     in  filter p xs
--     filter isMinimum xs
--   where isMinimum curr = min xs == curr 

number :: [(Int, Int)] -> Int
number xs = sum (map(deltaStop)  xs)

deltaStop :: (Int, Int) -> Int
deltaStop x = fst(x) - snd(x)



repeatStr :: Int -> String -> String
repeatStr 0 _  = ""
repeatStr n str = str ++ repeatStr (subtract 1 n) str



boolToWord :: Bool -> String
boolToWord x 
  | x == True  = "Yes"
  | x == False = "No"


  makeNegative :: (Num a, Ord a) => a -> a
  makeNegative x
    | x < 0 = x
    | x > 0 = x*(-1)
    | otherwise = 0