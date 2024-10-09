module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
[] +++ lst = lst
(x:xs) +++ lst = x : (xs +++ lst)

reverse2 :: [a] -> [a]
reverse2 list = if (null list)
  then []
  else reverse2 (tail list) +++ [head list]

maxmin :: Ord a =>[a] -> (a, a)
maxmin [x] = (x, x)
maxmin (x:xs) = ( if x > max_rest then x else max_rest,
                  if x < min_rest then x else min_rest)
                  where (max_rest, min_rest) = maxmin xs

fibonacci :: Integer -> Integer
fibonacci n = case n of
                0 -> 0
                1 -> 1
                _ -> fibonacci (n - 1) + fibonacci (n - 2)

myNull :: [a] -> Bool
myNull [] = True
myNull (_:_) = False

myHead :: [a] -> a
myHead [a] = a
myHead (a:_) = a

myTail :: [a] -> [a]
myTail [_] = []
myTail (_:as) = as


sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:r@(y:_)) = x < y && sorted(r)

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0 = Nothing
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n | otherwise =  let Just n1 = ifibonacci (n - 1)
                                Just n2 = ifibonacci (n - 2)
                            in Just (n1 + n2)

binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom x y | x == y = 1
binom n k = (binom (n - 1) (k - 1)) + (binom (n - 1) k)


multipleOf :: Integer -> Integer -> Bool
multipleOf a b = (mod a b) == 0

specialMultiples :: Integer -> String
specialMultiples n
  | multipleOf n 2 = show n ++ " is a multiple of 2"
  | multipleOf n 3 = show n ++ " is a multiple of 3"
  | multipleOf n 5 = show n ++ " is a multiple of 5"
  | otherwise = show n ++ " is a beautiful number"

-- Exercise 2.6
ackerman :: Integer -> Integer -> Integer
ackerman m n
  | m == 0 && n >= 0 = n + 1
  | m > 0 && n == 0 = ackerman (m - 1) 1
  | m > 0 && n > 0 = (ackerman (m - 1) (ackerman m (n - 1)))

unzip' :: [(a, a)] -> ([a], [a])
unzip' [] = ([], [])
unzip' ((x, y) : rest) = (x : xr, y : yr)
  where (xr, yr) = unzip' rest

-- end of Exercise 2.6