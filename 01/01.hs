import Test.QuickCheck

-- Exercise 1

nAnd_01 :: Bool -> Bool -> Bool
nAnd_01 x y = not(x && y)

nAnd_02 :: Bool -> Bool -> Bool
nAnd_02 False _ = True
nAnd_02 _ x = not x

nAnd_03 :: Bool -> Bool -> Bool
nAnd_03 False False = True
nAnd_03 False True = True
nAnd_03 True False = True
nAnd_03 True True = False

-- Exercise 2

prop_nAnd :: Bool -> Bool -> Bool
prop_nAnd x y = 
    nAnd_01 x y == nAnd_02 x y && 
    nAnd_02 x y == nAnd_03 x y &&
    nAnd_03 x y == nAnd_04 x y

nAnd_04 :: Bool -> Bool -> Bool
nAnd_04 True True = False
nAnd_04 _ _ = True

-- Exercise 3

nDigits :: Integer -> Int
nDigits num = length(show(abs(num)))

-- Exercise 4

nRoots :: Float -> Float -> Float -> Int
nRoots a b c
    | a == 0    = error "The first argument should be non-zero!"
    | disc > 0  = 2
    | disc == 0 = 1
    | disc < 0  = 0
    where
        disc = b^2 - 4.0 * a * c

-- Exercise 5

calculateRoots :: Float -> Float -> Float -> (Float, Float)
calculateRoots a b c = (x1, x2)
    where
        disc = b^2 - 4.0 * a * c
        x1 = (-b + sqrt(disc))/ 2 * a
        x2 = (-b - sqrt(disc))/ 2 * a

smallerRoot:: Float -> Float -> Float -> Float
smallerRoot a b c
    | roots == 0 = error "The quadratic equation has no roots!"
    | roots == 1 = x1
    | roots == 2 = min x1 x2
    where
        roots = nRoots a b c
        (x1, x2) = calculateRoots a b c

largerRoot:: Float -> Float -> Float -> Float
largerRoot a b c
    | roots == 0 = error "The quadratic equation has no roots!"
    | roots == 1 = x1
    | roots == 2 = max x1 x2
    where
        roots = nRoots a b c
        (x1, x2) = calculateRoots a b c

-- Exercise 6

power2 :: Integer -> Integer
power2 num
    | num == 0 = 1
    | num > 0  = power2(num - 1) * 2
    | otherwise = 0

-- Exercise 7

mult :: Integer -> Integer -> Integer
mult _ 0 = 0
mult 0 _ = 0
mult x y
    | x < 0 && y < 0 = mult u v
    | x < 0 || y < 0 = mult u v * (-1)
    | otherwise      = mult x (y - 1) + x
    where
        u = abs x
        v = abs y

-- Exercise 8

prod :: Integer -> Integer -> Integer
prod 0 _ = 0
prod _ 0 = 0
prod x y
    | x == y = y
    | x < y = prod x (y - 1) * y
    | otherwise = error "The first argument cannot be lower than the second argument!"

fac :: Integer -> Integer
fac n
    | n == 0 = 1
    | n > 0 = prod 1 n
    | otherwise = error "Only defined for natural numbers"