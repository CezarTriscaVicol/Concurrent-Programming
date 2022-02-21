import io.threadcso._
import scala.collection.immutable.Set

class Sync{
	private val push, pop = ManyOne[Int]

	def enter(id: Int) = push!id
	def exit(id: Int) = pop!id

	def server = proc {
		var set = Set[Int]()
		var sum = 0
		serve(
			((sum%3==0) && push) =?=> {x => set = set + x; sum = sum + x}
			| pop =?=> {x => assert(set(x)); set = set - x; sum = sum - x}
		)
	} 

	server.fork
	/** Shut down the stack, terminating the server thread. */
	def shutdown = { push.close; pop.close }
}