import NN
import Utils
import Node
import Layer


testInput nn trainInput = do
    print (getOutput 
                (feedForward 
                    (setInput 
                         nn trainInput)))

main = do
    print "create NN"
    let trainInput = [1.0, 1.0]
    let teacherSignals = [1.0]
    let nn = setInput (createNN 2 10 1 2.5) trainInput

    print "train 100 steps"
    let nn2 = (trainStep nn trainInput teacherSignals 100)
    print nn2
    
    print "testing values [1.0, 1.0]"
    testInput nn2 trainInput
    
    print "done"



