import io.threadcso._
import scala.collection.mutable.Queue

class PairingServer{
	private type ReplyChan = Chan[String]
	// Waiting queues for the clients
	val manQueue, womanQueue = new Queue[(String, ReplyChan)]()
	// Channels through which clients send their names to the controller
	val toWomanController, toManController = ManyOne[(String, ReplyChan)]
	
	// Public methods defined in the Question.
	def manSync(me: String): String = {
        val replyChan = OneOne[String]
		toManController!(me, replyChan)
		replyChan?()
	}
	
	def womanSync(me: String): String = {
        val replyChan = OneOne[String]
		toWomanController!(me, replyChan)
		replyChan?()
	}

    // Procedure that will decide which clients get paired
	private def controller = proc("contoller"){
		serve(
			toWomanController =?=> {
				x => x match { case (womanName: String, womanReplyChan: ReplyChan) => {
					if(manQueue.nonEmpty){           // We immediately pair this process if we can.
						assert(!womanQueue.nonEmpty) // We can't have processes waiting in both queues.
						val (manName, manReplyChan) = manQueue.dequeue
						womanReplyChan!  manName 
					  	  manReplyChan!womanName
						womanReplyChan.close
					  	  manReplyChan.close
					}else{ // Otherwise, we put it in the queue.
						womanQueue.enqueue((womanName, womanReplyChan))
					}
				}}
			}
			|
			toManController =?=> {
				x => x match { case (manName: String, manReplyChan: ReplyChan) => {
					if(womanQueue.nonEmpty){       // We immediately pair this process if we can.
						assert(!manQueue.nonEmpty) // We can't have processes waiting in both queues.
						val (womanName, womanReplyChan) = womanQueue.dequeue
						womanReplyChan!  manName
					  	  manReplyChan!womanName 
						womanReplyChan.close
					  	  manReplyChan.close
					}else{ // Otherwise, we put it in the queue.
						manQueue.enqueue((manName, manReplyChan))
					}
				}}
			}
		)
	}

	controller.fork
} 