import io.threadcso._

/** Various things shared between the sequential and concurrent smoothing
  * functions. */
object Smooth{
  type Row = Array[Boolean] // of size m
  type Image = Array[Row] // of size N

  /** Test if majority of neightbours of b(i)(j) are set. */
  def majority(b: Image, i: Int, j: Int): Boolean = {
    val n = b.length
    var sum = 0 // # set neighbours so far
    var count = 0 // # neighbours so far
    for(i1 <- i-1 to i+1; if i1 >= 0 && i1 < n;
        j1 <- j-1 to j+1; if j1 >= 0 && j1 < n){
      count += 1; if(b(i1)(j1)) sum += 1
    }
    2*sum >= count
  }

  /** Print the image in a. */
  def printArray(a: Smooth.Image) = {
    val n = a.length
    for(i <- 0 until n){
      for(j <- 0 until n) if(a(i)(j)) print("*") else print(" ");
      println;
    }
    println;
  }
}

// -------------------------------------------------------

/** A sequential smoothing algorithm. */
class SmoothSequential(a: Array[Array[Boolean]], maxIters: Int){
  private val n = a.length
  assert(a.forall(_.length == n))

  def apply() = {
    var done = false; var iters = 0

    while(!done && iters < maxIters){
      done = true
      val newA = Array.ofDim[Boolean](n, n)
      for(i <- 0 until n; j <- 0 until n){
	newA(i)(j) = Smooth.majority(a, i, j)
	done &&= (newA(i)(j) == a(i)(j))
      }
      iters += 1
      for(i <- 0 until n) a(i) = newA(i) // update for next round
    }
  }
}
      
// -------------------------------------------------------

/** Smooth image a, using p workers, with at most maxIters iterations. */
class SmoothShared(a: Array[Array[Boolean]], p: Int, maxIters: Int){

	val nThreads = p
	val nRows = a.length
	val nCols = a(0).length
	val rowsPerThread = nRows / nThreads

	var newImage = Array.fill(nRows, nCols)(false) 

	val barrier = new TreeBarrier(p+1)

	var changed = true

	type Row = Array[Boolean]
	type Image = Array[Row]

	def checker = proc("Checker") {
		var cnt = 0
		while(changed && cnt < maxIters) {
			barrier.sync(nThreads)
			changed = false
			barrier.sync(nThreads)
			barrier.sync(nThreads)
			cnt += 1
		}
	}

	def thread(me: Int, startRow: Int, lastRow: Int) = proc("Worker"+me.toString) {
		var cnt = 0
		while(changed && cnt < maxIters) {
			barrier.sync(me)
			for (i <- startRow until lastRow) 
				for(j <- 0 until nCols) 
					newImage(i)(j) = Smooth.majority(a, i, j)
			barrier.sync(me)
			for (i <- startRow until lastRow) 
				for(j <- 0 until nCols) {
					if(a(i)(j) != newImage(i)(j))
						changed = true
					a(i)(j) = newImage(i)(j)
				}
			barrier.sync(me)
			cnt += 1
		}
	}

	def apply() = {
		val threads = || (for (i <- 0 until nThreads) yield thread(i, i*rowsPerThread, (i+1)*rowsPerThread))  
		run(threads || checker)
	}
}

// -------------------------------------------------------

import scala.util.Random

/** Test of SmoothShared, comparing it with the sequential implementation. */
object SmoothSharedTest{
  val n = 40 // size of image
  val p = 10 // # workers
  val maxIters = 40 // max # iterations

  /** Do a single test. */
  def doTest = {
    val a = Array.fill(n,n)(Random.nextFloat >= 0.55)
    val a1 = a.map(_.clone)
    new SmoothShared(a, p, maxIters)()
    new SmoothSequential(a1, maxIters)()
    assert((0 until n).forall(i => a(i).sameElements(a1(i))),
           { Smooth.printArray(a); Smooth.printArray(a1); n })
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){
      doTest; if(true || i%10 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}
