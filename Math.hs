
module Math
   (sigmoid
    , listSquared
    , listSum
    )
where

-- sigmoid()
e = exp 1
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + e**(-x))


-- listSquared()
listSquared :: [Double] -> [Double]
listSquared l = map (\n -> n * n) l

-- sumList()
listSum :: [Double] -> Double
listSum l = foldl (+) 0.0 l
