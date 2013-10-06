import NN
import Utils
import Node
import Layer


testInput nn trainInput = do
    print (getOutput 
                (feedForward 
                    (setInput 
                         nn trainInput)))


train trainInput teacherSignals = do
    -- print "create NN and train 100 steps"
    let nn = (trainStep nnNew trainInput teacherSignals 3000)
                where nnNew = setInput (createNN 2 10 1 2.5) trainInput

    -- print nn 
    testInput nn trainInput

main = do
    -- only training one set of data at a time...
    print "testing values [1.0, 1.0] => 1.0"
    train [1.0, 1.0] [1.0]
    
    print "testing values [0.0, 0.0] => 0.0"
    train [0.0, 0.0] [0.0]

    print "testing values [1.0, 0.0] => 0.0"
    train [1.0, 0.0] [0.0]

    print "testing values [0.0, 1.0] => 0.0"
    train [0.0, 1.0] [0.0]

