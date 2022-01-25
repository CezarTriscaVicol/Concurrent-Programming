import ox.gavin.experiments.{Experiments,Graphs}
import io.threadcso._

/** An object that performs a single observation. */
object MatrixRun{

  type Matrix = Array[Row]
  type Row = Array[Double]

  /* We'll create random matrices, with values uniform in [0..maxVal) */
  val random = scala.util.Random
  val MaxVal = 100

  /** Random Double in [-max, max). */
  def uniform(max: Double): Double = max*(2*random.nextDouble-1)

  /** Create a random row of length n. */
  def mkRow(n: Int): Row = 
    Array.fill(n)(uniform(MaxVal))

  /** Create a random matrix of size n*n. */
  def mkMatrix(n: Int): Matrix = 
    Array.fill(n)(mkRow(n))

  def pickParams(n: Int): (Matrix, Matrix) = {
    val a = mkMatrix(n); val b = mkMatrix(n)
    (a, b)
  }

  def main(args: Array[String]) = {
    var n = -1 // size of matrix
    var p = -1 // # workers
    var reps = 1 // # repetitions
    var numTasks = -1

    var i = 0
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "-n" => n = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--numTasks" => numTasks = args(i+1).toInt; i += 2
      case arg => println("Unrecognised argument: "+arg); sys.exit
    }
    assert(p > 0)
    val (a,b) = pickParams(n)

    val start = System.nanoTime
    val matrix_bag = new MatrixBagObjects(a, b, n, nWorkers=p, nTasks=numTasks)
    for(_ <- 0 until reps) matrix_bag()
    val duration_bag:Double = System.nanoTime - start
    val matrix_seq = new SeqMatrixMul(a, b, n)
    for(_ <- 0 until reps) matrix_seq()
    val duration_seq:Double = System.nanoTime - start - duration_bag
    println("Concurrent time: "+(duration_bag/1000000)*(10/reps.toDouble)+" ms")
    println("Sequential time: "+(duration_seq/1000000)*(10/reps.toDouble)+" ms")
    io.threadcso.exit()
  }
}

