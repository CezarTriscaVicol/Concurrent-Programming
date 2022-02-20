import io.threadcso._
import scala.util.Random

/** A simple implementation of a barrier using a tree-like structure. */
class TreeBarrier(p: Int) {
	private val up, down = Array.fill(p)(OneOne[Boolean])

	private val Synced = true;

  	def sync(me: Int) = { 
  		val child1 = 2*me+1; val child2 = 2*me+2 // Identities of children
			// Receive sync values from both children
			if (child1 < p) assert(up(child1)?())
			if (child2 < p) assert(up(child2)?())	
			// Send sync values to parent, and wait for answer
			if (me != 0){
				up(me)!Synced
				assert(down(me)?())
			}
			// Send sync value to children
			if (child1 < p) down(child1)!Synced
			if (child2 < p) down(child2)!Synced
			// Release current node
  	}

  	def shutdown = { 
  		  up.map(a => a.close)
  		down.map(a => a.close)
  	}
}

object TreeBarrierTest{
  /** Number of repetitions. */
  val reps = 10000

  /** A thread. */
  def thread(me: Int, nSyncs: Int, barrier: TreeBarrier, stack: ServerStack[Int]) = proc("Thread"+me) {
    for(i <- 0 until nSyncs){
    	stack.push(i)
    	barrier.sync(me)
    }
  }

  def checker(nThreads: Int, nSyncs: Int, stack: ServerStack[Int]) = proc("Checker") {
  	for(i <- 0 until nSyncs){
  		for(j <- 0 until nThreads){
  			assert(stack.pop == Some(nSyncs-i-1))
  		}
  	}
  	assert(stack.pop == None)
  }

  /** Run a single test. 
    * n threads.         */
  def runTest(nThreads: Int, nSyncs: Int) = {
  	val treeBarrier = new TreeBarrier(nThreads)
  	val checkStack = new ServerStack[Int]
    val threads = || (for(i <- 0 until nThreads) yield thread(i, nSyncs, treeBarrier, checkStack))
    run(threads)
  	run(checker(nThreads, nSyncs, checkStack))
  	treeBarrier.shutdown
  	checkStack.shutdown
  }

  def main(args : Array[String]) = {
    // Run tests
    for(r <- 0 until reps){
      runTest(16, 100)
      print(".")
    }
    println
    io.threadcso.exit()
  }
}

