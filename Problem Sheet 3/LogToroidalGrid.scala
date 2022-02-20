import io.threadcso._
import scala.util.Random
/** Implementation of MinMax using a ring, with a distinguished initiator.
  * The initiator is the thread with identity 0. */
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

object LogGridMaxTest{
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

