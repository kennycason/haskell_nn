module Layer
   (Layer(..)
    ,createLayer
    ,createEmptyLayer
    ,calculateErrors
    ,calculateOutputErrors
    ,adjustWeights
    ,clearLayerValues
    ,calculateNodeValues
    ,sigmoidLayerValues
    ,isOutputLayer
    ,getErrors
)
where

import Utils
import Node

data Layer = Layer {  
                nodes :: [Node]
                ,errors :: [Double]
                ,teacherSignals :: [Double]
                ,learningRate :: Double
            } deriving Show


createNodeRow :: Int -> Int -> [Node]
createNodeRow numNodes numWeightsPerNode = replicate numNodes (createNode numWeightsPerNode 0.5)


createLayer :: Int -> Int -> Double -> Layer
createLayer numNodes numWeightsPerNode theLearningRate =
        Layer {
              nodes = (createNodeRow numNodes numWeightsPerNode)
              ,errors = (replicate numNodes 0.0)
              ,teacherSignals = (replicate numNodes 0.0)
              ,learningRate = theLearningRate
        }


createEmptyLayer = createLayer 0 0 0

-- calculateErrors()
normalizeValue :: Node -> Double
normalizeValue node = (value node) * (1.0 - (value node))

sumError :: Node -> Layer -> Double
sumError node childLayer = foldl (+) 0 (zipWith (*) (errors childLayer) (weights node))

calculateNodeErrors :: Node -> Layer -> Double
calculateNodeErrors node childLayer = (sumError node childLayer) * (normalizeValue node)

calculateErrors :: Layer -> Layer -> Layer
calculateErrors layer childLayer | isOutputLayer layer = calculateOutputErrors layer
                                 | otherwise = layer { 
                                            errors = map (\node -> calculateNodeErrors node childLayer) (nodes layer)
                                        }
  

-- calculateOutputErrors()
calculateOutputNodeError :: Node -> Double -> Double
calculateOutputNodeError node teacherSignal = (teacherSignal - (value node)) 
                                                          * ((value node) * (1.0 - (value node)))
           
calculateOutputErrors :: Layer -> Layer
calculateOutputErrors layer = layer {
                                errors = zipWith (\node teacherSignal -> 
                                                        calculateOutputNodeError node teacherSignal)
                                                                                     (nodes layer)
                                                                                     (teacherSignals layer)
                            }
        
                       
-- adjustWeights()
adjustWeightValue :: Double -> Double -> Double -> Double -> Double
adjustWeightValue value weight error learningRate =  weight + (learningRate * error * value)

adjustNodeWeight :: Node -> Layer -> Double -> Node
adjustNodeWeight node childLayer learningRate = node { 
                                                 weights = zipWith 
                                                      (\weight error -> 
                                                              adjustWeightValue (value node) weight error learningRate)
                                                                            (weights node)
                                                                            (errors childLayer) 
                                               }

adjustWeights :: Layer -> Layer -> Layer
adjustWeights layer childLayer = layer { 
                                    nodes = map (\node -> adjustNodeWeight 
                                                                    node 
                                                                    childLayer 
                                                                    (learningRate layer)) 
                                                                                   (nodes layer) 
                                }


-- clearAllValues()
clearLayerValues :: Layer -> Layer
clearLayerValues layer = layer { nodes = (map clearNodeValue (nodes layer)) }


-- calculateNodeValues()
sumOfWeightsValues :: Layer -> [Double]
sumOfWeightsValues layer = (foldl1 (zipWith (+)) [multConstList (value node) (weights node) | node <- (nodes layer)])

updateChildNodeValue :: Double -> Node -> Node
updateChildNodeValue weightedValue childNode = childNode {
                                                value = (value childNode) + weightedValue
                                             }

calculateNodeValues :: Layer -> Layer -> Layer
calculateNodeValues layer childLayer = childLayer {
                                        nodes = zipWith updateChildNodeValue (sumOfWeightsValues layer) (nodes childLayer)
                                     }

-- sigmoidLayerValues()
sigmoidLayerValues :: Layer -> Layer
sigmoidLayerValues layer = layer { nodes = map (\node -> sigmoidNodeValue node) (nodes layer) }

-- isOutputLayer()
isOutputLayer :: Layer -> Bool
isOutputLayer layer = null (weights (getFirstNode layer))


-- getFirstNode()
getFirstNode :: Layer -> Node
getFirstNode layer = head (nodes layer)

-- getErrors()
getErrors :: Layer -> [Double]
getErrors layer = (errors layer)
    

