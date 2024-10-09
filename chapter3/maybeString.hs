{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

import DataModel

maybeString (Just _) = "Just"
maybeString Nothing = "Nothing"

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\case "Alejandro" -> "Welcome, Writer"
                            name        -> "Welcome, " ++ name
                    ) names

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> x * n

