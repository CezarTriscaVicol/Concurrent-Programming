import io.threadcso._
import scala.language.postfixOps
import scala.collection.mutable.Queue

object BoundedBuff{
  private val nWorkers = 10

  /** Bounded buffer. */
  def boundedBuff[T](in: ?[T], out: ![T], n: Int): PROC = proc{
    val queue = new Queue[T]()
    val mutex = MutexSemaphore()
    val size      = CountingSemaphore(0)
    val remaining = CountingSemaphore(n)
    run(proc{
      while(true){
        remaining.down
        val aux = in?()
        mutex.down
        queue.enqueue(aux)
        mutex.up
        size.up
      }
    } || proc{
      while(true){
        size.down
        mutex.down
        val aux = queue.dequeue()
        mutex.up
        remaining.up
        out!aux
      }
    })
  }  
    
  val c1 = ManyOne[Int]
  val c2 = OneMany[Int]
  // Random delay
  def pause = Thread.sleep(scala.util.Random.nextInt(500))

  def  inputWorker(me: Int): PROC = proc( "inputThread"+me){ var n = 0; while (true) { pause; c1!n; n+=1 }}
  def outputWorker(me: Int): PROC = proc("outputThread"+me){ Thread.sleep(1000); repeat{ println(c2?); pause } }
  def system = {
    val  inputWorkers = || (for (i <- 0 until nWorkers) yield  inputWorker(i))
    val outputWorkers = || (for (i <- 0 until nWorkers) yield outputWorker(i))
    boundedBuff(c1, c2, 10) || inputWorkers || outputWorkers
  }

  def main(args: Array[String]) = {
    system()
  }
}