filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = case f x of
    False -> filter' f xs
    True -> x : (filter' f xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ i [] = i
foldr' f i (x:xs) = f x (foldr' f i xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ i [] = i
foldl' f i (x:xs) = foldl' f (f i x) xs
