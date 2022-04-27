import scala.util.Random
import io.threadcso._


object PairingMonitorTest{
  	private val rand = new Random()
  	private var nameLength = 30
  	private var nWorkers = 100
	private var nNames   = 100
  
 	private def nextAlphanumeric(length: Int): String = Random.alphanumeric.take(length).mkString

	val monitor = new PairingMonitor

    // Channels for workers (senders) to send their collected pairs to the collector.
	private val toWomenCollector, toMenCollector = ManyOne[List[(String, String)]]
	// Global list for all of the pairs, from both women's and men's side.
    private var womenList, menList = List[(String, String)]()

    // Procedure that collects all of the pairs.
	def collector = proc("collector"){
    	serve(
    		((menList.size != nWorkers * nNames)   && toMenCollector)   =?=> {
    			x => menList = x ++: menList
    		}
    		|
    		((womenList.size != nWorkers * nNames) && toWomenCollector) =?=> {
    			x => womenList = x ++: womenList
    		}
    	)
		toWomenCollector.close
		toMenCollector.close
	}

    // Worker that creates n men names and sends them.
    // All sendMen workers have different certificates
    def sendMen(certificate:String, n: Int) = proc("sendMen:" + certificate){
        var returnList: List[(String, String)] = List()
    	for(_ <- 0 until n){
    		val manName = certificate + nextAlphanumeric(nameLength)
    		val womanName = monitor.manSync(manName)
    		returnList = (manName, womanName) :: returnList
    	}
    	toMenCollector!returnList
    }

    // Worker that creates n women names and sends them.
    // All sendWomen workers have different certificates
    def sendWomen(certificate:String, n: Int) = proc("sendWomen:" + certificate){
        var returnList: List[(String, String)] = List()
    	for(_ <- 0 until n){
    		val womanName = certificate + nextAlphanumeric(nameLength)
    		val manName = monitor.womanSync(womanName)
    		returnList = (manName, womanName) :: returnList
    	}
    	toWomenCollector!returnList
    }

    def comparator(a: (String, String), b: (String, String)): Boolean = {
    	if(a._1 == b._1)
    		a._2 < b._2
    	else
    		a._1 < b._1
    }
 
	def main(args: Array[String]) = {	
		var i = 0
    	while(i < args.length) args(i) match{
      		case "--nWorkers" => nWorkers = args(i+1).toInt; i += 2
      		case "--nNames" => nNames = args(i+1).toInt; i += 2
      		case "--namelength" => nameLength = args(i+1).toInt; i += 2
      		case arg => println("Unrecognised argument: "+arg+"\n"); sys.exit
    	}
   		val   menWorkers = || (for (i <- 0 until nWorkers) yield   sendMen((10000+i).toString, nNames))
   		val womenWorkers = || (for (i <- 0 until nWorkers) yield sendWomen((10000+i).toString, nNames))
        run(menWorkers || womenWorkers || collector)
        /** println(  menList)
            println(womenList) */
          menList =   menList.sortWith(comparator) // We sort both lists and then compare them.
        womenList = womenList.sortWith(comparator)
        while(menList.size>0){
        	if(menList.size % 10 == 0)
        		print('.')
        	assert(menList.head == womenList.head) // Make sure that all pairs are the same.
        	  menList =   menList.tail
        	womenList = womenList.tail
        }
        println()
    	io.threadcso.exit()
  	}
}