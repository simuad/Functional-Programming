import Data.List ( sort )
import Data.Char ( toUpper, isLetter )

-- Exercise 1

average :: [Float] -> Float
average xs = sum xs / fromIntegral(length(xs))

-- Exercise 2

divides_01 :: Integer -> [Integer]
divides_01 0 = error "All non-zero integers are divisors of 0"
divides_01 num 
    | num > 0 = [y | y <- [1 .. num `div` 2] ++ [num], num `mod` y == 0]
    | otherwise = divides_01(abs(num))

divides_02 :: Integer -> [Integer]
divides_02 0 = error "All non-zero integers are divisors of 0"
divides_02 num
    | num > 0 = divides_02' num (num `div` 2) [num]
    | otherwise = divides_02(abs(num))
    where
        divides_02' _ 1 xs = 1:xs
        divides_02' num currDiv divisors
            | num `mod` currDiv == 0 = divides_02' num (currDiv - 1) (currDiv:divisors)
            | otherwise = divides_02' num (currDiv - 1) divisors

isPrime :: Integer -> Bool
isPrime num
    | num <= 0  = False
    | otherwise = divides_01 num == [1, num]

-- Exercise 3

prefix :: String -> String -> Bool
prefix st1 st2 = st1 == take (length st1) st2

substring :: String -> String -> Bool
substring st (x:xs)
    | length st > length (x:xs) = False
    | otherwise = prefix st (x:xs) || substring st xs

-- Exercise 4

permut :: [Integer] -> [Integer] -> Bool
permut xs ys = sort(xs) == sort(ys)

-- Exercise 5

capitalise :: String -> String
capitalise st = [toUpper c | c <- st , isLetter c]

-- Exercise 6

itemTotal :: [(String, Float)] -> [(String, Float)]
itemTotal basket = itemTotal' [] basket
    where
        itemTotal' mergedBasket [] = mergedBasket
        itemTotal' mergedBasket ((item, price):xs) = itemTotal' (mergedItem:mergedBasket) remainingList
            where
                (mergedItem, remainingList) = merge item price xs
                merge item currPrice remainingList = merge' item currPrice [] remainingList
                    where
                        merge' item currPrice remainingList [] = ((item, currPrice), remainingList)
                        merge' item currPrice remainingList ((name, price):xs)
                            | item == name = merge' item (currPrice + price) remainingList xs
                            | otherwise = merge' item currPrice ((name, price):remainingList) xs

itemDiscount :: String -> Integer -> [(String, Float)] -> [(String, Float)]
itemDiscount _ _ [] = []
itemDiscount item discount ((name, price):xs)
    | discount < 0 || discount > 100 = error "Discount must be between 0% and 100%"
    | item == name = (name, price * discountDecimal):itemDiscount item discount xs
    | otherwise = (name, price):itemDiscount item discount xs
    where
        discountDecimal = fromIntegral(100 - discount) / 100