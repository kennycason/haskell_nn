module Node
   (Node(..), 
    numWeights, 
    createNode, 
    compareNode,
    sigmoid)
where

data Node = Node { value::Double, weights::[Double] } deriving Show

createNode :: Int -> Double -> Node
createNode numNodes defaultWeight = Node 0.0 (replicate numNodes defaultWeight)

numWeights :: Node -> Int
numWeights node = length (weights node)

compareNode :: Node -> Node -> Double
compareNode n1 n2 = abs ((value n2) - (value n1))

e = exp 1
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + e**(-x))
