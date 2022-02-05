import io.threadcso._

class RingFold[T](xs: Array[T], f: (T, T) => T, out: Array[Chan[T]]){
  private val nThreads = xs.size

    private val forwardChan = Array.fill(nThreads)(OneOne[T])
  // forwardChan(i) sends messages from i to i+1

  private def thread(me: Int): PROC = proc {
    if(me == 0){
      forwardChan(0)!xs(0)
      val result = forwardChan(nThreads-1)?()
      forwardChan(0)!result
      forwardChan(0).close
      out(0)!result
      out(0).close
    }else{
      val current_val = f(forwardChan(me-1)?(), xs(me))
      forwardChan(me)!current_val
      val result = forwardChan(me-1)?()
      if(me != nThreads-1){
        forwardChan(me)!result
      }
      forwardChan(me).close
      out(me)!result
      out(me).close
    }
  }

  def apply(): PROC = { || (for (i <- 0 until nThreads) yield thread(i)) }
}