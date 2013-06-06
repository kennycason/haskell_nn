module NN
   (NN(..),  
    createNN,
    feedForward,
    clearAllValues,
    backPropagate,
    calculateError,
    setTeacherSignals,
    trainStep, 
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

trainStep :: NN -> [Double] -> [Double] -> NN

trainStep nn trainInput trainOutput =
        ({-clearAllValues.backPropagate-}feedForward)
                        (setTeacherSignals (setInput nn trainInput) trainOutput)
                    

-- setInput()
setNodeInput :: Node -> Double -> Node
setNodeInput node newValue = node { value = newValue }

mapInputLayer :: Layer -> [Double] -> Layer
mapInputLayer layer values = layer {
                                       nodes = zipWith (\node value -> setNodeInput node value) 
                                                             (nodes layer)  values
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
                                          output = setLayerTeacherSignals (output nn) teacherSignals
                                        }


-- createNN()
createNN :: Int -> Int -> Double -> NN
createNN numInput numOutput learningRate = NN { input = (createLayer numInput 3 learningRate),
                hidden = (createLayer 3 numOutput learningRate),
                output = (createLayer numOutput 0 learningRate)
              }


-- feedForward()
feedForward :: NN -> NN
feedForward nn = nn {
                    hidden = calculateNodeValues (input nn) (hidden nn),
                    output = calculateNodeValues (hidden nn) (output nn)
                 }

clearAllValues :: NN -> NN
clearAllValues nn = nn {
                        input = clearLayerValues (input nn),
                        hidden = clearLayerValues (hidden nn),
                        output = clearLayerValues (output nn)
                    }


-- calculateError()
listSquared :: [Double] -> [Double]
listSquared l = map (\n -> n * n) l

sumList :: [Double] -> Double
sumList l = foldl (+) 0.0 l

calculateError :: NN -> Double
calculateError nn = sumList (
                     listSquared ( 
                           zipWith (\node teacherSignal -> (value node) - teacherSignal) 
                                           (nodes (output nn))  (teacherSignals (output nn))

                          ))

-- backPropagate()
backPropagate :: NN -> NN
backPropagate nn = nn {
                    output = calculateErrors (output nn) createEmptyLayer, 
                    hidden = adjustWeights (calculateErrors (hidden nn) (output nn)) (output nn),
                    input = adjustWeights (input nn) (hidden nn)
                  }

