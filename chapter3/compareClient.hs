import DataModel

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual { person = p1 }) (Individual { person = p2 })
    = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)

