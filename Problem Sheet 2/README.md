Question 3:

Compile and run using:

    scalac UnboundedBuffer.scala
    scala UnboundedBuff

Question 4:

Compile and run using:

    scalac Adaptive.scala AdaptiveTest.scala Trapezium.scala TrapeziumTest.scala
    scala AdaptiveTest
    
Question 5:

Compile and run using:

     scalac PairingServer.scala PairingServerTest.scala
     scala PairingServerTest
     
Question 6:

Compile and run using:

    scalac Ring.scala RingTest.scala
    scala RingTest
    
If f were commutative, we would have a bit more liberty in choosing how we go about our Ring threads. Currently, we need to do two loops around the ring and send a total of 2n messages. However, if f((f(x,y),z) = f(x,f(y,z)), we can do a single concurrent loop, sending a total of O(n^2) messages.

Question 7:

(a) The given scheme doesn't check whether all of the processes are passive at the same time, but checks whether they are passive sequentially. This means that with the processes activate in a ring-like order, it is very likely that this scheme will terminate them, despite always having one process active at all times.

(b) We could maybe use a dedicated process that keeps track of the states of all the other processes. This would force the N processes to communicate with this main process every time they happen to change their state, which might slow down the program. The main process would use something like serve(..) to make sure that all processes get a chance to change their status fairly quickly (fulfilling the liveness property). The main process would terminate all other processes only after seeing all of them as passive at the same time and not having any unfulfilled status changes (fulfilling a safety property). ]

(c) Instead of having the token only go one-way, we could have it go both ways around. I think this would mostly guarantee that all processes must be passive if both of them come back true.
