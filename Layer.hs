module Layer
   (Layer(..),  
    createLayer, 
    createEmptyLayer,
    calculateErrors,
    adjustWeights,
    clearLayerValues,
    calculateNodeValues,
    isOutputLayer
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
              (replicate numWeightsPerNode 0.0) -- errors
              (replicate numNodes 0.0) -- teacher signals
              learningRate


createEmptyLayer = createLayer 0 0 0


-- general helper(s)
listProduct = zipWith (*)

sigmoidNodeValue :: Node -> Node
sigmoidNodeValue node = node { value = sigmoid (value node) }


-- calculateErrors()
sumNodeError :: Node -> Layer -> Double
sumNodeError node childLayer = foldl (+) 0 (listProduct (weights node) (errors childLayer))

calculateNodeError :: Node -> Layer -> Double
calculateNodeError node childLayer = (sumNodeError node childLayer) * (value node) * (1.0 - (value node))

calculateErrors :: Layer -> Layer -> Layer
calculateErrors layer childLayer = layer { 
                                      errors = map (\node -> calculateNodeError node childLayer) (nodes layer)
                                   }
        
                       
-- adjustWeights()
adjustWeightValue :: Double -> Double -> Double -> Double
adjustWeightValue learningRate value error = value + (learningRate * value * error)

adjustNodeWeight :: Node -> Layer -> Node
adjustNodeWeight node childLayer = node { 
                 weights = (map 
                       (\error -> adjustWeightValue (learningRate childLayer) (value node) error) 
                       (errors childLayer)
                 )}

adjustWeights :: Layer -> Layer -> Layer
adjustWeights layer childLayer = layer { nodes = map (\node -> adjustNodeWeight node childLayer) (nodes layer) }


-- clearAllValues()
clearNodeValue :: Node -> Node
clearNodeValue node = Node 0.0 (weights node)

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


-- calculateOutputNodeValues()
-- calculateOutputNodeValues :: Layer -> Layer
-- calculateOutputNodeValues layer = layer { nodes = map sigmoidNodeValue (nodes layer) }  


-- isOutputLayer()
isOutputLayer :: Layer -> Bool
isOutputLayer layer = null (weights (getFirstNode layer))


-- getFirstNode()
getFirstNode :: Layer -> Node
getFirstNode layer = head (nodes layer)
    











