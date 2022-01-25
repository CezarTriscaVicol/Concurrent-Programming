import io.threadcso._

/** Object to test the concurrent Trapezium rule code. */
object MatrixTest{
  /** We will test the Matrix class by selecting random matrices.  Each
    * matrix will be represented by an array of arrays of its values. */ 
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

  /** Convert a row to a string. */
  def toString(a: Row): String =
    "["+(0 until a.length).map(i => a(i).toString+", ").mkString(" ")+"]"

  /** Convert a poly to a string. */
  def toString(a: Matrix): String =
    (0 until a.length).map(i => a(i).toString).mkString("\n")

  /** Pick parameters for a test.
    * @return a tuple (a, b, n, nWorkers) indicating that the product of a and b
    * should be estimated using nWorkers workers */
  def pickParams: (Matrix, Matrix, Int, Int) = {
    val n = 1+random.nextInt(20)
    // matrices to multiply
    val a = mkMatrix(n); val b = mkMatrix(n)
    // Number of workers
    val nWorkers = 1+random.nextInt(16)
    (a, b, n, nWorkers)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 100000000){
      val (a, b, n, nWorkers) = pickParams
      val seqResult = new SeqMatrixMul(a, b, n)()
      val concResult = {
          var nTasks = 1+random.nextInt(n*n)
          if(nTasks <= n){
            while(n%nTasks != 0)
              nTasks = nTasks + 1
          }else{
            while((n*n)%nTasks != 0 || nTasks%n != 0)
              nTasks = nTasks + 1
          }
          assert(0 < nTasks && nTasks <= n*n)
          new MatrixBagObjects(a, b, n, nWorkers, nTasks)()
        }
      // println(seqResult+"; "+concResult)
      for(i <- 0 until n)
       for(j <- 0 until n)
        assert(
          Math.abs((seqResult(i)(j)-concResult(i)(j))/seqResult(i)(j)) < 1E-7 ||
           Math.abs(seqResult(i)(j)-concResult(i)(j)) < 1E-10,
         "failed\nf = "+toString(a)+"\n"+toString(b)+"\n"+
            "i = "+i+"; j = "+j+"; n = "+n+"; nWorkers = "+nWorkers+"\n"+
            "seqResult = "+seqResult(i)(j)+"; concResult = "+concResult(i)(j))
      
      if(i < 1000 || i%1000 == 0){
          print("."); if (i%100000 == 0) print(i)
      }
    }
    io.threadcso.exit()
  } 

}
