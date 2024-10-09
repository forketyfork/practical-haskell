{-# LANGUAGE LambdaCase #-}

import Data.List

data InfNumber a = MinusInfinity
    | Number a
    | PlusInfinity
    deriving Show

infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity _ = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number(max a b)

minSort :: [Integer] -> [Integer]
minSort = unfoldr (\case [] -> Nothing
                         xs -> Just (m, delete m xs) where m = minimum xs)
