{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Chapter2.DataTypes where

import Data.Char

data Client = GovOrg        String
              | Company     String Integer Person String
              | Individual  Person Bool
  deriving Show

data Person = Person String String Gender
  deriving Show

data Gender = Male | Female | Unknown
  deriving Show

data Manufacturer = Manufacturer String String
  deriving Show

data TravelOptions = TravelOptions Bool Bool
  deriving Show

data TimeMachine = TimeMachine Manufacturer Integer String TravelOptions Float
  deriving Show

clientName :: Client -> String
clientName client = case client of
                      GovOrg  name                    -> name
                      Company name _ _ _              -> name
                      Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm

clientName2 :: Client -> Maybe String
clientName2 client = case client of
                      Company name _ _ _  -> Just name
                      _                   -> Nothing


-- Exercise 2-5
data GenderStats = GenderStats Integer Integer Integer
  deriving Show

(+++) :: GenderStats -> GenderStats -> GenderStats
(GenderStats m1 f1 u1) +++ (GenderStats m2 f2 u2) = GenderStats (m1 + m2) (f1 + f2) (u1 + u2)

genderStats :: [Person] -> GenderStats
genderStats [] = GenderStats 0 0 0
genderStats [Person _ _ Male] = GenderStats 1 0 0
genderStats [Person _ _ Female] = GenderStats 0 1 0
genderStats [Person _ _ Unknown] = GenderStats 0 0 1
genderStats (x : xs) = (genderStats [x]) +++ (genderStats xs)

discount :: [TimeMachine] -> Float -> [TimeMachine]
discount [] _ = []
discount ((TimeMachine m i s t price) : xs) d = (TimeMachine m i s t (price * d)) : (discount xs d)

-- end of Exercise 2-5

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Sergei") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

data ClientR = GovOrgR {clientRName :: String }
  | CompanyR {
    clientRName :: String,
    companyId :: Integer,
    person :: PersonR,
    duty :: String }
  | IndividualR {
    person :: PersonR
  }
  deriving Show

data PersonR = PersonR {
  firstName :: String,
  lastName :: String
}
  deriving Show

greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn } } = "Hi, " ++ fn
greet CompanyR { clientRName = c } = "Hi, " ++ c
greet GovOrgR { } = "Welcome"

greet' :: ClientR -> String
greet' IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName
greet' CompanyR { .. } = "Hi, " ++ clientRName
greet' GovOrgR { } = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR {firstName = initial:rest }) =
  let capitalized = (toUpper initial) : rest
  in p { firstName = capitalized }
nameInCapitals p@(PersonR {firstName = ""}) = p

-- Exercise 2-7
data TimeMachineR = TimeMachineR {
  manufacturer :: Manufacturer,
  model :: Integer,
  name :: String,
  travelOptions :: TravelOptions,
  price :: Float
}
  deriving Show

discount' :: [TimeMachineR] -> Float -> [TimeMachineR]
discount' [] _ = []
discount' (t@(TimeMachineR { .. }) : xs) d = (t { price = price * d }) : (discount' xs d)
-- End of Exercise 2-7

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | TimeOut Integer
data Connection

connect :: String