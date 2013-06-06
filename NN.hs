module NN
   (NN(..),  
    createNN,
    feedFoward,
    backPropagate,
    calculateError,
    setTeacherSignals,
    getOutput,
    setInput
)
where

import Math
import Layer
import Node

data NN = NN { 
               input :: Layer,
               hidden :: Layer, 
               output :: Layer
             } deriving Show

-- setInput()
setNodeInput :: Node -> Double -> Node
setNodeInput node newValue = node { value = newValue }

mapInputLayer :: Layer -> [Double] -> Layer
mapInputLayer layer values = layer { 
                                       nodes = [ (setNodeInput node value) 
                                               | node <- (nodes layer) 
                                               , value <- values 
                                               ]  
                                   }

setInput :: NN -> [Double] -> NN
setInput nn values = nn { input = mapInputLayer (input nn) values }


-- getOutput()
getOutput :: NN -> [Double]
getOutput nn = map (\node -> value node) (nodes (output nn))


-- setTeacherSignals()
setLayerTeacherSignals :: Layer -> [Double] -> Layer
setLayerTeacherSignals layer newTeacherSignals = layer { teacherSignals = newTeacherSignals }

setTeacherSignals :: NN -> [Double] -> NN
setTeacherSignals nn teacherSignals = nn { 
                                          input = setLayerTeacherSignals (input nn) teacherSignals,
                                          hidden = setLayerTeacherSignals (hidden nn) teacherSignals,
                                          output = setLayerTeacherSignals (output nn) teacherSignals
                                        }


-- createNN()
createNN :: Double -> NN
createNN learningRate = NN { input = (createLayer 2 10 learningRate),
                hidden = (createLayer 10 1 learningRate),
                output = (createLayer 1 0 learningRate)
              }

-- feedFoward()
feedFoward :: NN -> NN
feedFoward nn = nn {
                    input = calculateNodeValues (input nn) (hidden nn),
                    hidden = calculateNodeValues (hidden nn) (output nn),
                    output = calculateOutputNodeValues (output nn)
                 }


-- calculateError()
listDiff :: [Double] -> [Double] -> [Double]
listDiff a b = zipWith (-) a b

listSquared :: [Double] -> [Double]
listSquared l = map (\n -> n * n) l

sumList :: [Double] -> Double
sumList l = foldl (+) 0.0 l

calculateError :: NN -> Double
calculateError nn = sumList (
                         listSquared (
                                 listDiff 
                                       (teacherSignals (output nn)) 
                                       (teacherSignals (output nn)) ))


-- backPropagate()
backPropagate :: NN -> NN
backPropagate nn = nn {
                    output = calculateErrors (output nn) createEmptyLayer, 
                    hidden = adjustWeights (calculateErrors (hidden nn) (output nn)) (output nn),
                    input = adjustWeights (input nn) (hidden nn)
                 }
