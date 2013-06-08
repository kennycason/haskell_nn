import NN
import Utils
import Node
import Layer

printNN nn = do
    print "Showing NN"
    print nn
    print "Input Nodes:"
    print (length (nodes (input nn)))
    print "Hidden Nodes:"
    print (length (nodes (hidden nn)))
    print "Output Nodes:"
    print (length (nodes (output nn)))

main = do
    let teacherSignals = [1.0]
    let trainInput = [1.0, 1.0]

    print "create NN"
    let nn =  (createNN 2 1 0.2) 
    printNN nn    

    print "train 100 steps"
    let nn2 = (trainStep nn trainInput teacherSignals 100)
    printNN nn2
    

    print "input [1.0, 1.0]"
    let nn3 = feedForward (setInput nn2 [1.0, 1.0])
    printNN nn3
    print "output"
    print (getOutput nn3)
    print "done"



