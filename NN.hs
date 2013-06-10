module NN
    (NN(..)
    ,createNN
    ,feedForward
    ,clearAllValues
    ,backPropagate
    ,calculateError
    ,setTeacherSignals
    ,trainStep
    ,getOutput
    ,setInput
)
where

import Utils
import Layer
import Node

data NN = NN { 
               input :: Layer
               ,hidden :: Layer
               ,output :: Layer
             } deriving Show


-- createNN()
createNN :: Int -> Int -> Int -> Double -> NN
createNN numInput numCenterNodes numOutput learningRate = NN { 
                input = (createLayer numInput numCenterNodes learningRate)
                ,hidden = (createLayer numCenterNodes numOutput learningRate)
                ,output = (createLayer numOutput 0 learningRate)
              }


-- trainStep()
trainSingleStep :: NN -> [Double] -> [Double] -> NN
trainSingleStep nn trainInput trainOutput = 
                (clearAllValues.backPropagate.feedForward)
                        (setTeacherSignals (setInput nn trainInput) trainOutput)

trainStep :: NN -> [Double] -> [Double] -> Int-> NN
trainStep nn trainInput trainOutput 1 = trainSingleStep nn trainInput trainOutput
trainStep nn trainInput trainOutput cycles = 
                             trainStep 
                                    (trainSingleStep nn trainInput trainOutput) 
                                    trainInput 
                                    trainOutput 
                                    (cycles-1)


-- setInput()
mapInputLayer :: Layer -> [Double] -> Layer
mapInputLayer layer values = layer {
                                       nodes = zipWith (\node newValue -> node {value = newValue}) 
                                                             (nodes layer)  values
                                   }

setInput :: NN -> [Double] -> NN
setInput nn values = nn { input = mapInputLayer (input nn) values }


-- getOutput()
getOutput :: NN -> [Double]
getOutput nn = map (\node -> value node) (nodes (output nn))


-- setTeacherSignals()
setLayerTeacherSignals :: Layer -> [Double] -> Layer
setLayerTeacherSignals layer newTeacherSignals = layer { 
                                                    teacherSignals = newTeacherSignals
                                                    ,errors = (replicate (length newTeacherSignals) 0.0)
                                                    }

setTeacherSignals :: NN -> [Double] -> NN
setTeacherSignals nn teacherSignals = nn { 
                                          output = setLayerTeacherSignals (output nn) teacherSignals
                                    }


-- feedForward()
feedForward :: NN -> NN
feedForward nn = let updatedHidden = sigmoidLayerValues (calculateNodeValues (input nn) (hidden nn))
                 in nn {
                    hidden = updatedHidden
                    ,output =  sigmoidLayerValues
                                    (calculateNodeValues 
                                              updatedHidden (output nn))
                 }


-- clearAllValues()
clearAllValues :: NN -> NN
clearAllValues nn = nn {
                        input = clearLayerValues (input nn)
                        ,hidden = clearLayerValues (hidden nn)
                        ,output = clearLayerValues (output nn)
                    }


-- calculateError()
calculateError :: NN -> Double
calculateError nn = sumList (
                     listSquared ( 
                           zipWith (\node teacherSignal -> (value node) - teacherSignal) 
                                           (nodes (output nn))  (teacherSignals (output nn))

                          )) / (fromIntegral (length (nodes (output nn))))


-- backPropagate()
backPropagate :: NN -> NN
backPropagate nn = let updatedOutput = calculateErrors (output nn) createEmptyLayer
                       updatedHidden = adjustWeights (calculateErrors (hidden nn) updatedOutput) (output nn)
                   in nn {
                    output = updatedOutput
                    ,hidden = updatedHidden
                    ,input = adjustWeights (input nn) updatedHidden
                  }

