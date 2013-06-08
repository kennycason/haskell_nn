import NN
import Math
import Node
import Layer

main = do
    let teacherSignals = [1.0]

    let trainInput = [1.0, 1.0]

    print "create NN"
    print "set input and teacherSignals"
    let nn =  (createNN 2 1 0.2) 
    print nn

    print "train 1 step"
    let nn2 = (trainStep nn trainInput teacherSignals 20)
    print nn2

    print (getOutput nn2)
    print "input [1.0, 1.0]"
    let nn3 = feedForward (setInput nn2 [1.0, 1.0])
    print "output"
    print (getOutput nn3)
    print "calculate errors"
    print (getErrors (calculateOutputErrors (output nn3)))

    print "done"



