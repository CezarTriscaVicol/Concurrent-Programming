import io.threadcso._

/** A class to search Graph g concurrently. */
class ConcGraphSearch[N](g: Graph[N]) extends GraphSearch[N](g){
  /**The number of workers. */
  val numWorkers = 8

  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[N] = {
    // Queue storing edges and the path leading to that edge
    val stack = new TerminatingPartialStack[N](numWorkers)
    stack.push(start)

    // Channel on which a worker tells the coordinator that it has found a
    // solution.
    val solutionFound = ManyOne[N]

    // A single worker
    def worker = proc("worker"){
      repeat{
        val n = stack.pop
        for(n1 <- g.succs(n)){
          if(isTarget(n1)) solutionFound!(n1) // done!
          else stack.push(n1)
        }
      }
      solutionFound.close // causes coordinator to close down
    }

    // Variable that ends up holding the result; written by coordinator. 
    var result: Option[N] = None

    def coordinator = proc("coordinator"){
      attempt{ result = Some(solutionFound?()) }{ }
      stack.shutdown // close queue; this will cause most workers to terminate
      solutionFound.close // in case another thread has found solution
    }

    val workers = || (for(_ <- 0 until numWorkers) yield worker)
    (workers || coordinator)()
    result
  }
}
