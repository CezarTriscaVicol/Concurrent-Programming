import scala.util.Random
import io.threadcso._


object RingTest{
    private val random = new Random()

    private def nextAlphanumeric(length: Int): String = Random.alphanumeric.take(length).mkString

    def stringReceiver(result: String, in: ?[String]) = proc("receiver"){
        assert(result == in?())
    }

    def doStringTest: PROC ={
        val nThreads = 2+random.nextInt(200)
        val xs = Array.fill(nThreads)(nextAlphanumeric(5))
        val out = Array.fill(nThreads)(OneOne[String])
        var sum = ""
        for(i <- 0 until nThreads)
            sum += xs(i)
        val ring = new RingFold[String](xs, (x, y) => (x + y), out)()
        val threads = || (for (i <- 0 until nThreads) yield stringReceiver(sum, out(i)))
        ring || threads
    }

    def intReceiver(result: Int, in: ?[Int]) = proc("receiver"){
        assert(result == in?())
    }

    def doIntTest: PROC ={
        val nThreads = 2+random.nextInt(200)
        val xs = Array.fill(nThreads)(1+random.nextInt(2000))
        val out = Array.fill(nThreads)(OneOne[Int])
        var sum = 0
        for(i <- 0 until nThreads)
            sum += xs(i)
        val ring = new RingFold[Int](xs, (x, y) => (x + y), out)()
        val threads = || (for (i <- 0 until nThreads) yield intReceiver(sum, out(i)))
        ring || threads
    }

	def main(args: Array[String]) = {	
        for(i <- 1 to 1000){
            if(i % 10 == 0)print('.')
            doIntTest()
        } 
        println()
        for(i <- 1 to 1000){
            if(i % 10 == 0)print('.')
            doStringTest()
        } 
        println()
        io.threadcso.exit()
  	}
}