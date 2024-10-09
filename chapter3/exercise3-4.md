The source code for the map function, according to [documentation](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Base.html#map):

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

The source code for `mapAsFold`:
```haskell
mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x l -> f x : l) []
```

*Base case*: the list is empty,
```haskell
map _ [] = []
mapAsFold [] = foldr(\x l -> f x : l) [] [] = []
```

Assuming we've already proven that:
```haskell
map f xs = mapAsFold f xs
```

*Induction step*:
```haskell
map f (x:xs) = f x : map f xs
mapAsFold f (x:xs) = foldr(\x l -> f x : l) [] (x:xs)
    = (\x l -> f x : l) x (foldr (\x l -> f x : l) [] xs)
    = f x : (foldr (\x l -> f x : l) [] xs)
    = f x : mapAsFold f xs
    = f x : map f xs
```

Proof for `map f . map g  = map (f . g)`

*Base case*: the list is empty,
```haskell
map f (map g []) = map f [] = []
map (f . g) [] = []
```

*Induction step*:
```haskell
map f (map g i:is) =
    map f ((g i) : (map g is)) =
    (f . g i) : (map f (map g is))

map (f . g) (i:is) =
    (f . g i) : (map (f . g) is)

map f (map g is) = map (f . g) is 
(by induction)

q.e.d.
```    
