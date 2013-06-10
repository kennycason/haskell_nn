module Node
    (Node(..)
    ,numWeights
    ,createNode
    ,compareNode
    ,sigmoidNodeValue
    ,clearNodeValue
    )
where

import Utils

data Node = Node { value::Double, weights::[Double] } deriving Show

-- sigmoidNodeValue()
sigmoidNodeValue :: Node -> Node
sigmoidNodeValue node = node { value = sigmoid (value node) }


-- clearNodeValue()
clearNodeValue :: Node -> Node
clearNodeValue node = Node 0.0 (weights node)


-- createNode()
createNode :: Int -> Double -> Node
createNode numNodes defaultWeight = Node {
                                        value = 0.0 
                                        ,weights = replicate numNodes defaultWeight
                                        }


-- numWeights()
numWeights :: Node -> Int
numWeights node = length (weights node)


-- compareNode()
compareNode :: Node -> Node -> Double
compareNode n1 n2 = abs ((value n2) - (value n1))

