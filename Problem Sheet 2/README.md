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
