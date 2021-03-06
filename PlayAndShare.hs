-- Play and share --
-- divisibleBy
divisibleBy :: Int -> Int -> Bool
divisibleBy divisor dividend =  if dividend `mod` divisor == 0 then True else False
-- divisibleByFive
divisibleByFive = divisibleBy 5

-- isDivisibleByFive
isDivisibleByFive :: [Int] -> [Bool]
isDivisibleByFive xs = foldr(\x acc -> (x `mod` 5 == 0):acc) [] xs

-- bigCubes
bigCubes xs = filter (>500) $ map (^3) xs 

-- lottaBiggest
-- lottaBiggest xs =  maximum xs: $maximum xs: $maximum xs: maximum$xs : []
-- powers
powers num = map ($num) [(^2),(^3),(^4)]

 
