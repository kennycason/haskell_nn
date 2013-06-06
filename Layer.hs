module Layer
   (Layer(..),  
    createLayer, 
    createEmptyLayer,
    calculateErrors,
    adjustWeights,
    clearAllValues,
    calculateNodeValues,
)
where

import Math
import Node


data Layer = Layer {  
                nodes :: [Node], 
                errors :: [Double], 
                teacherSignals :: [Double],
                learningRate :: Double
            } deriving Show

createNodeRow :: Int -> Int -> [Node]
createNodeRow numNodes numWeightsPerNode = replicate numNodes (createNode numWeightsPerNode 0.5)

createLayer :: Int -> Int -> Double -> Layer
createLayer numNodes numWeightsPerNode learningRate =
        Layer (createNodeRow numNodes numWeightsPerNode) -- nodes
              (replicate numWeightsPerNode 0.3) -- errors
              (replicate numNodes 0.2) -- teacher signals
              learningRate

createEmptyLayer = createLayer 0 0 0

-- general helper(s)
listProduct = zipWith (*)

-- calculateErrors()
sumNodeError :: Node -> Layer -> Double
sumNodeError node childLayer = foldl (+) 0 (listProduct (weights node) (errors childLayer))

calculateNodeError :: Node -> Layer -> Double
calculateNodeError node childLayer = sumNodeError node childLayer * (value node) * (1.0 - (value node))

calculateErrors :: Layer -> Layer -> Layer
calculateErrors layer childLayer = layer { 
                                      errors = map (\node -> calculateNodeError node childLayer) (nodes layer)
                                   }
                               
-- adjustWeights()
sumAdjustWeight :: Double -> Double -> Double -> Double
sumAdjustWeight learningRate value error = value + (learningRate * value * error)

adjustNodesWeight :: Node -> Layer -> Node
adjustNodesWeight node childLayer = node { 
                 weights = (map 
                       (\error -> sumAdjustWeight (learningRate childLayer) (value node) error) 
                       (errors childLayer)
                 )}

adjustWeights :: Layer -> Layer -> Layer
adjustWeights layer childLayer = layer { nodes = map (\node -> adjustNodesWeight node childLayer) (nodes layer) }

-- clearAllValues()
clearNodeValue :: Node -> Node
clearNodeValue node = Node 0.0 (weights node)

clearAllValues :: Layer -> Layer
clearAllValues layer = layer { nodes = (map clearNodeValue (nodes layer)) }

-- calculateNodeValues()
getFirstNode :: Layer -> Node
getFirstNode layer = head (nodes layer)

sigmoidNodeValue :: Node -> Node
sigmoidNodeValue node = node { value = sigmoid (value node) }

calculateNodeValues :: Layer -> Layer
calculateNodeValues layer -- output layer
                          | null (weights (getFirstNode layer)) = layer { nodes = map sigmoidNodeValue (nodes layer) } 
                          -- other layers
                          | otherwise = layer { nodes = map sigmoidNodeValue (nodes layer) }  
    
















