import io.threadcso._
import scala.language.postfixOps
import scala.collection.mutable.Queue

/** Abstract class, representing the problem of calculating the integral of f
  * from a to b. */
class Adaptive(f: Double => Double, a: Double, b: Double, Epsilon: Double, nWorkers: Int){
  require(a <= b)

  /** Task consists of (a, b) */
  private type Task = (Double, Double)

  val queue = Queue[Task]((a,b))

  private var result: Double = 0

  private var toWorkers: Chan[Task] = OneMany[Task] 
  private var toDistributor: Chan[Task] = ManyOne[Task]
  private var toCollector: Chan[Option[Double]] = ManyOne[Option[Double]]

  private def worker = proc("worker"){
    repeat{
      val (a,b) = toWorkers?()
      val mid = (a+b)/2.0
      val fa = f(a); val fb = f(b); val fmid = f(mid)
      val lArea = (fa+fmid)*(mid-a)/2; val rArea = (fmid+fb)*(b-mid)/2
      val area = (fa+fb)*(b-a)/2
      if (Math.abs(lArea+rArea-area) < Epsilon) toCollector!Some(area)
      else { toCollector!None; toDistributor!(a,mid); toDistributor!(mid,b) }
    }
  }

  private def distributor = proc("distributor"){
    serve(
      (queue.nonEmpty && toWorkers) =!=> { queue.dequeue }
      | toDistributor =?=> { x => queue.enqueue(x) }
    )
  }

  private def collector = proc("collector"){
    var cnt: Int = 1
    while(cnt>0){
      val x: Option[Double] = toCollector?()
      x match {
        case Some(y) => { cnt -= 1 ; result += y }
        case None    => { cnt += 1 }
      }
    }
    toCollector.close
    toWorkers.close
    toDistributor.close
  }

  /** The main system. */
  private def system = {
    toWorkers = OneMany[Task]
    val workers = || (for (i <- 0 until nWorkers) yield worker)
    workers || distributor || collector
  }

  def apply: Double = { system(); result } 
}