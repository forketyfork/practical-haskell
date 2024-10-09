{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}

import DataModel
import Data.List
import Data.Function
import GHC.Exts

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
        sortBy (flip (compare `on` length)) .
        groupBy ((==) `on` duty) .
        filter isCompany
            where isCompany (Company {}) = True
                  isCompany _            = False

companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty)
    | client@(Company { .. }) <- clients
                           , then sortWith by duty
                           , then group by clientName using groupWith
                           , then sortWith by length client
                           ]
