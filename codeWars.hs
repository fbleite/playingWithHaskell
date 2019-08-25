-- module SquareDigit where
import Data.List (delete, isInfixOf, nub, sort)
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



-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence x = maxSequenceStart x 0

maxSequenceStart ::[Int] -> Int -> Int
maxSequenceStart [] s = s
maxSequenceStart (x:xs) s = if msi (x:xs) > s
                            then maxSequenceStart xs (msi (x:xs))
                            else maxSequenceStart xs s
                            where msi intx = maxSeqIndividual (length intx) (intx) 0

maxSeqIndividual :: Int -> [Int] -> Int -> Int
maxSeqIndividual 0 _ s= s
maxSeqIndividual l x s = if sum (take l x) > s
                        then maxSeqIndividual (l-1) x (sum (take l x))
                        else maxSeqIndividual (l-1) x s


groupByCommas :: Int -> String
groupByCommas  = groupByCommasString . show 

groupByCommasString :: String -> String
groupByCommasString (x:y:z:zs) = 
    let fullArray = x:y:z:zs
        arrayLengthMinus3 = subtract 3 . length $ fullArray
        last3Char = reverse . take 3 . reverse $ fullArray
        takeAllBut3 = take arrayLengthMinus3
        remainder = groupByCommasString . takeAllBut3 $ fullArray
    in remainder ++ "," ++ last3Char
groupByCommasString x = x



-- First version
inArray :: [String] -> [String] -> [String]
inArray a1 a2 = sort . nub . filter (`elemPartial` a2) $ a1

elemPartial :: String -> [String] -> Bool  
elemPartial a [] = False  
elemPartial a (x:xs)  
    | a `isInfixOf` x    = True  
    | otherwise = a `elemPartial` xs  


-- Fancier version
inArray' :: [String] -> [String] -> [String]
inArray' a1 a2 = sort . nub . filter (\s -> any (s `isInfixOf`) a2) $ a1


findNb :: Integer -> Integer
findNb volume = findNb' volume 0


findNb' :: Integer -> Integer -> Integer
findNb' volume iteration 
    | volume == 0 = iteration
    | volume < 0 = -1
    | otherwise = findNb' subtractedVolume newIteration
    where currentCubeVolume = (1+iteration) ^3
          subtractedVolume = volume - currentCubeVolume
          newIteration = iteration + 1


findEvenIndex :: [Int] -> Int
findEvenIndex arr = findEvenIndexRec arr 0

findEvenIndexRec :: [Int] -> Int -> Int
findEvenIndexRec array index
            | index == length array = -1
            | sumRight == sumLeft = index
            | otherwise = findEvenIndexRec array (index + 1)
            where reverseIndex = (length array) - index
                  sumRight = sum . take (reverseIndex -1) . reverse $ array
                  sumLeft = sum . take index $ array

