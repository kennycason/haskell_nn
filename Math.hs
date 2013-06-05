
module Math
   (sigmoid)
where


e = exp 1
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + e**(-x))
