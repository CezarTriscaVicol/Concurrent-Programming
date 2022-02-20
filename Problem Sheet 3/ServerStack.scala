import io.threadcso._

/** A total stack, implemented using a server. */
class ServerStack[T]{
	/** Channel for pushing. */
	private val pushC = ManyOne[T]
	/** Channel for popping. */
	private val popC = OneMany[Option[T]]
	/** Push x onto the stack. */
	def push(x: T) = pushC!x
	/** Optionally pop a value from the stack.
	* @return Some(x) where x is the value popped, or None if the stack is empty. */
	def pop: Option[T] = popC?()
	private def server = proc{
		val stack = new scala.collection.mutable.Stack[T]
		serve(
			pushC =?=> { x => stack.push(x) }
			| popC =!=> { if(stack.isEmpty) None else Some(stack.pop) }
		)
	}
	server.fork
	/** Shut down the stack, terminating the server thread. */
	def shutdown = { pushC.close; popC.close }
}