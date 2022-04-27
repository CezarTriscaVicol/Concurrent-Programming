import io.threadcso._

class PairingMonitor {
	private val monitor = new Monitor
	private val readyToStart, valueFilledByWoman, valueFilledByMan = monitor.newCondition
	private var   manStart = true
	private var womanStart = false
	private var value = ""
	def manSync(me: String): String = monitor.withLock {
		readyToStart.await(manStart) 
		manStart = false
		value = me; womanStart = true; valueFilledByMan.signal()
		valueFilledByWoman.await(!womanStart)
		manStart = true
		readyToStart.signal()
		return value
	}

	def womanSync(me: String): String = monitor.withLock {
	    valueFilledByMan.await(womanStart)
		val ret = value; value = me; womanStart = false; valueFilledByWoman.signal()
		return ret
	}
}