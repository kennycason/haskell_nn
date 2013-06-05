import Node
import Layer
import Math

main = do
    let n2 = createNode 10 0.5
    print n2

    let l = createLayer 2 10 0.2
    print l

    print (sigmoid 0.8)
    print "done"
