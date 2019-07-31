-- module SquareDigit where
import Data.List (delete)
import Numeric 
import Text.Printf   


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


noSpace :: String -> String
noSpace str = filter(isNotSpace) str
    
isNotSpace :: Char -> Bool
isNotSpace x = x/=' '


noSpace2 :: String -> String
noSpace2 = filter (/=' ')

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd n 
  | odd n = "Odd"
  | otherwise = "Even"


highAndLow :: String -> String
highAndLow input = 
    let stringToArray = map (read) . words $ input
        maxNum = maximum stringToArray
        minNum = minimum stringToArray
    in unwords [show (maxNum::Int), show (minNum::Int)] -- Resolving ambiguous type in the show function

    


seriesSum :: Integer -> String
seriesSum n =  formatFloatN . seriesSumNum $ n
    where formatFloatN floatNum  = showFFloat (Just 2) floatNum ""
    

seriesSumNum :: Integer -> Float
seriesSumNum 0 = 0
seriesSumNum n = calculation n + seriesSumNum (n-1)
    where calculation n = 1/(1 +((fromInteger n-1) * 3)) 



series :: [Double]
series = map (1/) [1, 4 ..]

seriesSum2 :: Integer -> String
seriesSum2 n = printf "%.2f" $ sum $ take (fromInteger n) series