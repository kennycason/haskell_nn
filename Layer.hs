module Layer
   (Layer(..),  
    createLayer, 
)
where

import Node

data Layer = Layer { 
                nodes::[Node], 
                errors::[Double], 
                teacherSignals::[Double],
                learningRate::Double
            } deriving Show

createNodeRow :: Int -> Int -> [Node]
createNodeRow numNodes numWeightsPerNode = (replicate numNodes (createNode numWeightsPerNode 0.5))

createLayer :: Int -> Int -> Double -> Layer
createLayer numNodes numWeightsPerNode learningRate =
        Layer (createNodeRow numNodes numWeightsPerNode) -- nodes
              (replicate numWeightsPerNode 1.0) -- errors
              (replicate numNodes 0.0) -- teacher signals
              learningRate

listProduct a b = zipWith (*) a b


sumNodeError :: Node -> Layer -> Double
sumNodeError node childLayer = foldl (+) 0 (listProduct (weights node) (errors childLayer))

calculateNodeError :: Node -> Layer -> Double
calculateNodeError node childLayer = (sumNodeError node childLayer) * (value node) * (1.0 - (value node))

{-
calculateErrors :: Layer -> Layer -> Layer
calculateErrors layer childLayer = Layer (nodes layer)
                                         (map calculateNodeError (nodes layer) childLayer ) -- need to map each of the nodes in the layer, and the childLayer to calculateNodeError
                                         (teacherSignals layer)
                                         (learningRate layer)
-}

-- (map even [1..10])

-- calculateErrors :: Layer -> Layer
-- calculateErrors =              
