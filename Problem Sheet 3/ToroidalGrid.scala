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

import io.threadcso._
import scala.util.Random

object GridMaxTest{
  /** Number of repetitions. */
  val reps = 10000
  val n = 20

  /** Array that will hold values chosen by threads, indexed by thread IDs. */
  var xss: Array[Array[Int]] = Array.fill(n, n)(0)

  def runTest(n: Int) = {
    var global = -1
    for(i <- 0 until n)
      for(j <- 0 until n){
        xss(i)(j) = Random.nextInt(100000)
        global = Math.max(global, xss(i)(j))
      }
    val grid = new GridMax(n, xss)()
    for(i <- 0 until n)
      for(j <- 0 until n)
        assert(global == grid(i)(j))
  }

  def main(args : Array[String]) = {
    // Run tests
    for(r <- 0 until reps){
      runTest(n)
      print(".")
    }
    println
    io.threadcso.exit()
  }
}

