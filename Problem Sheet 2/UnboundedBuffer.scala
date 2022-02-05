import io.threadcso._
import scala.language.postfixOps
import scala.collection.mutable.Queue

object UnboundedBuff{

  /** Unbounded buffer. */
  def unboundedBuff[T](in: ?[T], out: ![T]): PROC = proc{
    val queue = new Queue[T]()
    serve(
      (queue.nonEmpty && out) =!=> { queue.dequeue }
      | in =?=> { y => queue.enqueue(y) }
    )
  }  

  /** The complete system.
    * @param useAlt should the alt-based definition be used? */
  def system = {
    // Random delay
    def pause = Thread.sleep(scala.util.Random.nextInt(500))

    // We create a pipeline, consisting of (1) a process producing the natural
    // numbers; (2) a two-place buffer; (3) a process that echos to the
    // console.
    val c1, c2 = OneOne[Int]
 
    proc { var n = 0; while (true) { pause; c1!n; n+=1 }} || 
    unboundedBuff(c1, c2) || proc{ Thread.sleep(1000); repeat{ println(c2?); pause } }
  }

  def main(args: Array[String]) = {
    system()
  }
}
