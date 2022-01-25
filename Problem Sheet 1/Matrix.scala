import io.threadcso._
import scala.language.postfixOps

abstract class MatrixMulT(a: Array[Array[Double]], b: Array[Array[Double]], n: Int){
  require(a.size == n)
  require(b.size == n)

  /** Compute the matrix. */
  def apply(): Array[Array[Double]]

  /** Multiply row number row_a in a with column number col_b in b */
  protected def product(row_a: Int, col_b: Int)
      : Double = {
    require(row_a < n)
    require(col_b < n)
    var sum: Double = 0
    for(i <- 0 until n) 
      sum = sum + a(row_a)(i) * b(i)(col_b)
    sum
  }

  /** Multiply row numbers (left_a -> right_a) in a with column numbers (left_b -> right_b) in b
    * and return them in an sequence */
  protected def product(left_a: Int, right_a: Int, left_b: Int, right_b: Int)
      : Array[Array[Double]] = {
    require(left_a <= right_a)
    require(left_b <= right_b)
    var sum = Array.ofDim[Double](right_a - left_a + 1, right_b - left_b + 1)
    for(i <- left_a to right_a) 
      for(j <- left_b to right_b)
        sum(i - left_a)(j - left_b) = product(i, j)
    sum
  }
}

class SeqMatrixMul(a: Array[Array[Double]], b: Array[Array[Double]], n: Int)
    extends MatrixMulT(a, b, n){
  require(n > 0)

  def apply()  = product(0, n-1, 0, n-1)
}
// =======================================================


/** Class to calculated the multiplication of matrices a and b of size n, using nWorkers workers
  * processes, with a total of nTasks tasks.  This version
  * encapsulates the concurrency within objects. */
class MatrixBagObjects(a: Array[Array[Double]], b: Array[Array[Double]], n: Int, nWorkers: Int, nTasks: Int)
    extends MatrixMulT(a, b, n){
  require(0 < nTasks && nTasks <= n*n)

  /** Type of tasks to send to client.  The Task (left_a, right_a, left_b, right_b)
    * represents the task of calculating the matrix from rows left_a to right_a,
    * and columns left_b to right_b using taskSize intervals of columns. */
  private type Task = (Int, Int, Int, Int)

  private type TaskResult = (Int, Int, Int, Int, Array[Array[Double]])

  /** The bag of tasks object. */
  private class BagOfTasks{
    /** Channel from the controller to the workers, to distribute tasks. */
    private val toWorkers = OneMany[Task]

    /** Get a task.  
      * @throws Stopped exception if there are no more tasks. */
    def getTask: Task = toWorkers?

    /** A server process, that distributes tasks. */
    private def server = proc{
      if(nTasks >= n){
        val delta = (n*n)/nTasks
        for(i <- 0 until n){
         for(j <- 0 until (nTasks/n)){
            val left_b = j * delta
            require(left_b < n && left_b + delta - 1 < n)
            toWorkers!(i, i, left_b, left_b + delta - 1)
          }
        }
      }else{ // nTasks < n
        val delta = n/nTasks
        for(i <- 0 until nTasks){
          val left_a = i * delta
          require(left_a < n && left_a + delta - 1 < n)
          toWorkers!(left_a, left_a + delta - 1, 0, n - 1)
        }
      }
      toWorkers.close
    }

    // Start the server running
    server.fork
  }

  /** A collector object that receives sub-results from the workers, and adds
    * them up. */
  private class Collector{
    /** Channel from the workers to the controller, to return sub-results. */
    private val toController = ManyOne[TaskResult]

    /** Channel that sends the final result. */
    private val resultChan = OneOne[Array[Array[Double]]]

    /** A collector, that accumulates the sub-results. */
    private def server = proc{
      var c = Array.ofDim[Double](n, n)
      for(i_task <- 0 until nTasks){
        val (left_a, right_a, left_b, right_b, aux_matrix) = toController?;
        for(i <- left_a to right_a)
          for(j <- left_b to right_b)
            c(i)(j) = aux_matrix(i - left_a)(j - left_b)
      }
      resultChan!c
    }

    // Start the server running
    server.fork

    /** Add x to the result. */
    def add(x: TaskResult) = toController!x

    /** Get the result. */
    def get: Array[Array[Double]] = resultChan?
  }


  /** A worker, which repeatedly receives tasks from the BagOfTasks, estimates
    * the integral, and adds the result to the Collector. */
  private def worker(bag: BagOfTasks, collector: Collector) = proc{
    repeat{
      val (left_a, right_a, left_b, right_b) = bag.getTask
      val result = (left_a, right_a, left_b, right_b, product(left_a, right_a, left_b, right_b))
      collector.add(result)
    }
  }

  /** Compute the product. */
  def apply: Array[Array[Double]] = {
    val bag = new BagOfTasks; val collector = new Collector
    val workers = || (for (i <- 0 until nWorkers) yield worker(bag, collector))
    workers()
    collector.get
  }
}
