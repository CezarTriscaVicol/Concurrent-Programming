import io.threadcso._
/** Implementation of MinMax using a ring, with a distinguished initiator.
  * The initiator is the thread with identity 0. */
class GridMax(n: Int, xss: Array[Array[Int]]){
  require(n >= 1 && xss.length == n && xss.forall(_.length == n))

  private val verChan, horChan = Array.fill(n, n)(OneOne[Int])

  private val maxss = Array.fill(n,n)(0)

  private val barrier = new TreeBarrier(n*n)

  def worker(i: Int, j : Int, x: Int, readUp: ?[Int], writeUp: ![Int],
                                   readRight: ?[Int], writeRight: ![Int]) = proc{
    if(j == 0){ // This is the initiator
      // Start the communications going
      writeRight!x
      // Receive max back, and send them round
      val max = readRight?()
      writeRight!max
      // Receive them back at the end
      readRight?()
      maxss(i)(j) = max
    }else{
      // receive max so far, and pass on possibly updated min and max
      val max1 = readRight?()
      writeRight! Math.max(max1, x)
      // receive final max, and pass them on
      val max = readRight?()
      writeRight!max
      maxss(i)(j) = max
    }
    barrier.sync(i*n+j)
    if(i == n-1){ // This is the initiator
      // Start the communications going
      writeUp!maxss(i)(j)
      // Receive max back, and send them round
      val max = readUp?()
      writeUp!max
      // Receive them back at the end
      readUp?()
      maxss(i)(j) = max
    }else{
      // receive max so far, and pass on possibly updated min and max
      val max1 = readUp?()
      writeUp! Math.max(max1, maxss(i)(j))
      // receive final max, and pass them on
      val max = readUp?()
      writeUp!max
      maxss(i)(j) = max
    }
  }

  /** Run the system, and return array storing results obtained. */
  def apply(): Array[Array[Int]] = {
    val workers = || (for(i <- 0 until n) yield (|| (for (j <- 0 until n) yield 
       worker(i, j, xss(i)(j), verChan((i+1)%n)(j), verChan(i)(j), horChan(i)((j-1+n)%n), horChan(i)(j)))))
    run(workers)
    verChan.map(a => a.map(x => x.close))
    horChan.map(a => a.map(x => x.close))
    maxss
  }

}

class LogGridMax(n: Int, xss: Array[Array[Int]]){
  require(n >= 1 && xss.length == n && xss.forall(_.length == n))

  private val chan = List.fill(n, n)(ManyOne[Int])

  private val maxss = Array.fill(n,n)(0)

  private val barrier = new TreeBarrier(n*n)

  def worker(i: Int, j : Int, x: Int, read: ?[Int ], write: List[List [![ Int ]]]) = proc{
    val id = i*n+j // Identity of current node
    val child1 = id*2+1
    val child2 = id*2+2  // Identities of children
    val parent = id/2 - 1 + id%2 // Identity of parent
    maxss(i)(j) = x // Inititialize max value
    // Receive max values from both children
    if (child1 < n*n) {
      val x = read?()
      maxss(i)(j) = Math.max(maxss(i)(j), x)
    }
    if (child2 < n*n){
      val x = read?()
      maxss(i)(j) = Math.max(maxss(i)(j), x)
    }
    // Send max values to parent, and wait for answer
    if (id != 0){
      write(parent/n)(parent%n)!maxss(i)(j)
      maxss(i)(j) = read?()
    }
    // Send max value to children
    if (child1 < n*n) write(child1/n)(child1%n)!maxss(i)(j)
    if (child2 < n*n) write(child2/n)(child2%n)!maxss(i)(j)
    // Release current node
  }

  /** Run the system, and return array storing results obtained. */
  def apply(): Array[Array[Int]] = {
    val workers = || (for(i <- 0 until n) yield (|| (for (j <- 0 until n) yield 
       worker(i, j, xss(i)(j), chan(i)(j), chan))))
    run(workers)
    chan.map(a => a.map(x => x.close))
    maxss
  }

}

import scala.util.Random

/** Test for GridMax and LogGridMax. */
object GridMaxTest{
  /** Run a single test.
    * @param useLog should the logarithmic version be used? */
  def doTest(useLog: Boolean) = {
    val n = 2+Random.nextInt(10)
    val xss = Array.fill[Int](n, n)(Random.nextInt(1000))
    val results = if(useLog) new LogGridMax(n, xss)() else new GridMax(n, xss)()
    val expected = xss.map(_.max).max
    assert(results.forall(_.forall(_ == expected)))
  }

  /** Main method. */
  def main(args: Array[String]) = {
    val useLog = args.nonEmpty && args(0) == "--useLog"
    for(i <- 0 until 10000){
      doTest(useLog)
      if(i%100 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}

