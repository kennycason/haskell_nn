module Layer
   (Layer(..)
    ,createLayer
    ,createEmptyLayer
    ,calculateErrors
    ,calculateOutputErrors
    ,adjustWeights
    ,clearLayerValues
    ,calculateNodeValues
    ,isOutputLayer
    ,getErrors
)
where

import Math
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
multConstList :: Double -> [Double] -> [Double]
multConstList const list = map (const *) list

calculateChildNodeValue :: Node -> Node -> Double
calculateChildNodeValue node childNode =  (foldl (+) 
                                                  (value childNode)
                                                  (multConstList (value node) (weights node)) )

updateChildNode :: Node -> Double -> Node
updateChildNode node newValue = node { value = newValue }

calculateNodeValues :: Layer -> Layer -> Layer
calculateNodeValues layer childLayer = childLayer {
                                        nodes = zipWith 
                                                (\node childNode -> 
                                                        updateChildNode 
                                                                 childNode 
                                                                 (calculateChildNodeValue node childNode))
                                                (map sigmoidNodeValue (nodes layer))  (nodes childLayer)
                                   }


-- isOutputLayer()
isOutputLayer :: Layer -> Bool
isOutputLayer layer = null (weights (getFirstNode layer))


-- getFirstNode()
getFirstNode :: Layer -> Node
getFirstNode layer = head (nodes layer)

-- getErrors()
getErrors :: Layer -> [Double]
getErrors layer = (errors layer)
    

