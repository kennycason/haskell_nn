import NN
import Math
import Node
import Layer

main = do
    -- let n2 = createNode 10 0.5
    -- print n2

    -- let l = createLayer 2 10 0.7
    -- print l

    -- print "sigmoid(.05)"
    -- print (sigmoid 0.5)

    let teacherSignals = [1.0]

    let trainInput = [1.0, 1.0]

    print "create NN"
    print "set input and teacherSignals"
    let nn =  (createNN 2 1 0.2) 
    print nn

    print "error"
    print (calculateError nn)

    print (trainStep nn trainInput teacherSignals)

    print "output"
    print (getOutput nn)
    

    print "done"
