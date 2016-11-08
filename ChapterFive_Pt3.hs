-- Haskell Chapter 5 Part 3 --
-- Name: Andrew Wang
-- Date: 6 November 2016

module ChapterFive_Pt3 where

-- getUserName
getUserName :: [Char] -> [Char]
getUserName (x:xs) 
 | x /= '@' = x:(getUserName xs)
 | otherwise = []   

-- numAs
numAs :: (Num a, Ord a) => [a] -> Int
numAs scores = length(filter (\num -> num >= 90) scores)

-- totalDiscount
totalDiscount :: (Num a, Ord a) => a -> [a] -> a
totalDiscount discountPercent listOfPrices = foldl(\acc x -> (discountPercent * x) + acc) 0 listOfPrices

-- totalWithDiscount
totalWithDiscount :: (Num a, Ord a) => a -> [a] -> a
totalWithDiscount discountPercent listOfPrices = foldl(\acc x -> x - (discountPercent * x) + acc) 0 listOfPrices

-- discountedItems
discountedItems :: (Num a, Ord a) => a -> [a] -> [a]
discountedItems discountPercent listOfPrices =  foldr(\x acc -> (x - (discountPercent * x)):acc) [] listOfPrices

-- anyBigNumbers
anyBigNumbers :: (Num a, Ord a) => a -> [a] -> Bool
anyBigNumbers threshold listOfNums = foldl(\acc x -> if x > threshold then True else acc) False listOfNums

-- definitions of and 
{- Since we are starting from the right fold, it will always have some fixed length no matter how long
 - xs is. For the left fold, it is unknown precisely where the list will end and thus the program is
 - trying to catch up to the last element of the list which will never be feasile through 'repeat'.-} 

-- multTableRow 
multTableRow :: Int -> [Int]
multTableRow multiplier = map ($multiplier) [(*1), (*2), (*3), (*4), (*5), (*6), (*7), (*8), (*9), (*10)]

-- testInverses
f1 x = 2*x + 3
f2 x = x*x
f3 x = 0.5*x - 3/2

testInverses f1 f2 
 |(f1 . f2) 5 == (f2 . f1) 5  = True
 |otherwise = False