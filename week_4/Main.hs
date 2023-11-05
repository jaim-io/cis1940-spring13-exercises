{-
  Exercise 1: Wholemeal programming
  Reimplement each of the following functions in a more idiomatic
  Haskell style. Use wholemeal programming practices, breaking each
  function into a pipeline of incremental transformations to an entire
  data structure. Name your functions fun1’ and fun2’ respectively.
  
    1. fun1 :: [Integer] -> Integer
       fun1 [] = 1
       fun1 (x:xs)
       | even x = (x - 2) * fun1 xs
       | otherwise = fun1 xs
    2. fun2 :: Integer -> Integer
       fun2 1 = 0
       fun2 n | even n = n + fun2 (n ‘div‘ 2)
       | otherwise = fun2 (3 * n + 1)

  Hint: For this problem you may wish to use the functions iterate
  and takeWhile. Look them up in the Prelude documentation to see
  what they do.
  -}

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then div n 2 else 3 * n + 1)

{-
  Exercise 3: More folds!
  1. Implement a function
  
    xor :: [Bool] -> Bool
    
    which returns True if and only if there are an odd number of True
    values contained in the input list. It does not matter how many
    False values the input list contains. For example,
    
    xor [False, True, False] == True
    xor [False, True, False, False, True] == False
    
    Your solution must be implemented using a fold.

  2. Implement map as a fold. That is, complete the definition
  
    map’ :: (a -> b) -> [a] -> [b]
    map’ f = foldr ...
  
    in such a way that map’ behaves identically to the standard map
    function.
  
  3. (Optional) Implement foldl using foldr. That is, complete the definition
  
    myFoldl :: (a -> b -> a) -> a -> [b] -> a
    myFoldl f base xs = foldr ...
    
    in such a way that myFoldl behaves identically to the standard
    foldl function.
    
    Hint: Study how the application of foldr and foldl work out:
    
    foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
    foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn
  -}

-- 1.
xor :: [Bool] -> Bool
xor = foldl1 (\ a b -> (a || b) && not (a && b)) 

-- 2.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x y -> f x : y) []

-- 3.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

{-
  Exercise 4: Finding primes
  Read about the Sieve of Sundaram. Implement the algorithm us-
  of_Sundaram ing function composition. Given an integer n, your function should
  generate all the odd prime numbers up to 2n + 2.
  
    sieveSundaram :: Integer -> [Integer]
    sieveSundaram = ...
  
  To give you some help, below is a function to compute the Cartesian product of two lists. This is similar to zip, but it produces all
  possible pairs instead of matching up the list elements. For example,
  
    cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
  
  It’s written using a list comprehension, which we haven’t talked about
  in class (but feel free to research them).
    
    cartProd :: [a] -> [b] -> [(a, b)]
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]
  -}

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) 
                  . (*2) 
                  . (\ (i, j) -> i + j + 2 * i * j)) 
                  . filter (\(i, j)-> i + j  + 2 * i * j <= n) 
                  $ cartProd [1..n] [1..n] 