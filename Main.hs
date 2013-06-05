import Node
import Layer
import Math

main = do
    let n2 = createNode 10 0.5
    print n2

    let l = createLayer 2 10 0.2
    print l

    let l2 = calculateNodeValues l
    print l2
    print (sigmoid 0.5)
    print "done"
