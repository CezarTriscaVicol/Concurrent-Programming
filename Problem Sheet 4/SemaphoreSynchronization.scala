import io.threadcso._
import scala.collection.immutable.Set

class SemSync{
	private var currentSum = 0

	private val mutexEnter = BooleanSemaphore(available = true)
	private val mutex = BooleanSemaphore(available = true)

	def enter(id: Int) = {
		mutexEnter.down
		mutex.down
		assert(currentSum % 3 == 0)
		currentSum += id
		if(id % 3 == 0)
			mutexEnter.up
		mutex.up
	}
	
	def exit(id: Int) = {
		mutex.down
		currentSum -= id
		if(id % 3 != 0)
			mutexEnter.up
		mutex.up
	}

}