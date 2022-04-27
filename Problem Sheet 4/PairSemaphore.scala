import io.threadcso._

class PairingSemaphore {
	val mutex = BooleanSemaphore(available = true)
	val signal1, signal2, signal3, signal4 = BooleanSemaphore(available = false)
	private var value = ""
	def manSync(me: String): String = {
		mutex.down 
		value = me
		signal1.up; signal2.down
		signal3.up; signal4.down
		val ret = value
		mutex.up 
		return ret
	}

	def womanSync(me: String): String = {
		signal1.down; signal2.up 
		val ret = value; value = me
		signal3.down; signal4.up
		return ret
	}
}