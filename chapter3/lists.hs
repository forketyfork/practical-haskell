{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

import DataModel

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False; _ -> True })

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _ = False

checkAnalytics :: [Client a] -> (Bool, Bool)
checkAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

enum :: Int -> Int -> [Int]
enum a b | a > b        = []
enum a b                = a : enum (a+1) b

withPositions :: [a] -> [(Int,a)]
withPositions list = zip (enum 1 $ length list) list

duplicateOdds :: [Integer] -> [Integer]
duplicateOdds list = [ 2 * x | x <- list, odd x ]
