-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0     = []
  | otherwise = toDigits(div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n 
  | n < 0     = []
  | otherwise = mod n 10 : toDigitsRev(div n 10)

-- Exercise 2
intListLenght :: [Integer] -> Integer
intListLenght []     = 0
intListLenght (x:xs) = 1 + intListLenght xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) 
  | even (intListLenght xs + 1) = x*2 : doubleEveryOther xs  
  | otherwise                   = x : doubleEveryOther xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) 
  | x >= 10   = sumDigits(toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs  

-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 src dest aux = []
hanoi n src dest aux = hanoi (n-1) src aux dest ++ [(src, dest)] ++ hanoi(n-1) aux dest src
