import scala.util.Random
import io.threadcso._

object SemSyncTest{ 
  def thread(me: Int, sync: SemSync): PROC = proc("thread"+me){
    val value = scala.util.Random.nextInt(1000)
    sync.enter(value)
    sync.exit(value)
  }

  def doTest = {
    val sync = new SemSync
    (|| (for (i <- 0 until 1000) yield thread(i, sync)))()
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100000){
      doTest; 
      if(i%100 == 0) 
        print(".")
    }
    println; io.threadcso.exit
  }
}
