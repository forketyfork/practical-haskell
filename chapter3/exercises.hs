{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import DataModel
import Data.List

-- Exercise 3-2
filterOnes :: [Integer] -> [Integer]
filterOnes is = filter (\x -> x == 1) is 

filterANumber :: [Integer] -> Integer -> [Integer]
filterANumber as a = filter (\x -> x == a) as

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f xs = filter (not . f) xs

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs clients = filter isGovOrg clients
    where isGovOrg (GovOrg _ _) = True
          isGovOrg _ = False

filterGovOrgs' :: [Client a] -> [Client a]
filterGovOrgs' clients = filter (\case (GovOrg _ _) -> True
                                       _          -> False) clients

-- Exercise 3-3
productPM :: [Integer] -> Integer
productPM [x] = x
productPM (x:xs) = x * (productPM xs)

productFold :: [Integer] -> Integer
productFold x = foldr (*) 1 x

getClientName :: Client a -> String
getClientName Company { clientName } = clientName
getClientName (Individual _ Person { firstName, lastName }) = firstName ++ " " ++ lastName
getClientName GovOrg { clientName } = clientName

minimumClientPM :: [Client a] -> Client a
minimumClientPM [c] = c
minimumClientPM (c:cs) = if (length $ getClientName c) < (length $ getClientName rest) then c else rest
    where rest = minimumClientPM cs 

minimumClientFold :: [Client a] -> Client a
minimumClientFold [c] = c
minimumClientFold (c:cs) = foldl (\c1 c2 -> if (length $ getClientName c1) < (length $ getClientName c2) then c1 else c2) c cs

allPM :: [Bool] -> Bool
allPM [x] = x
allPM (x:xs) = x && (allPM xs)

allFold :: [Bool] -> Bool
allFold x = foldr (&&) True x

minimumBy :: (Ord a) => (a -> a) -> [a] -> a
minimumBy g xs = foldr1 (\x y -> if (g x) < (g y) then x else y) xs

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = case (find (==x) xs) of
                 Nothing -> False
                 Just _ -> True


