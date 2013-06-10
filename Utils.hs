
module Utils
    (sigmoid
    ,listProduct
    ,listSquared
    ,listSum
    ,sumList
    ,multConstList
    ,addConstList
    )
where

-- sigmoid()
e = exp 1
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + e**(-x))


-- listProduct()
listProduct a b = zipWith (*) a b


-- listSum()
listSum a b = zipWith (+) a b


-- listSquared()
listSquared :: [Double] -> [Double]
listSquared l = map (\n -> n * n) l


-- multConstList()
multConstList :: Double -> [Double] -> [Double]
multConstList const list = map (const *) list


-- addConstList()
addConstList :: Double -> [Double] -> [Double]
addConstList const list = map (const +) list


-- sumList()
sumList :: [Double] -> Double
sumList l = foldl (+) 0.0 l
