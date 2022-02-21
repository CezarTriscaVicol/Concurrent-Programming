import io.threadcso._

/** The trait for a Sleeping Tutor protocol. */
trait SleepingTutor{
  /** A tutor waits for students to arrive. */
  def tutorWait

  /** A student arrives and waits for the tutorial. */
  def arrive
  
  /** A student receives a tutorial. */
  def receiveTute

  /** A tutor ends the tutorial. */
  def endTeach
}

/** The trait for a Sleeping Tutor protocol. */
class SleepingTutorMonitor extends SleepingTutor{
  var tuteDone: Boolean = false
  var count: Int = 0
  /** A tutor waits for students to arrive. */
  def tutorWait = synchronized {
    while(count != 2) wait() // wait until both students have arrived
  }

  /** A student arrives and waits for the tutorial. */
  def arrive = synchronized {
    count += 1 
    notifyAll()  // notify tutor that student arrived
    while(count != 2) wait() // wait until both students have arrived
  }
  
  /** A student receives a tutorial. */
  def receiveTute = synchronized {
    while(!tuteDone) wait() // wait until tutorial is done
    count -= 1
    notifyAll() // notify tutor that student left
  }

  /** A tutor ends the tutorial. */
  def endTeach = synchronized {
    tuteDone = true // signal to students that tutorial is done
    notifyAll()
    while(count > 0) wait() // wait for students to leave
    tuteDone = false // reset value for tuteDone
  }
}

// =======================================================

import scala.util.Random

object SleepingTutorSimulation{
  private val st: SleepingTutor = new SleepingTutorMonitor

  def student(me: String) = proc("Student"+me){
    while(true){
      Thread.sleep(Random.nextInt(2000))
      println("Student "+me+" arrives")
      st.arrive
      println("Student "+me+" ready for tutorial")
      st.receiveTute
      println("Student "+me+" leaves")
    }
  }

  def tutor = proc("Tutor"){
    while(true){
      println("Tutor waiting for students")
      st.tutorWait
      println("Tutor starts to teach")
      Thread.sleep(1000)
      println("Tutor ends tutorial")
      st.endTeach
      Thread.sleep(1000)
    }
  }

  def system = tutor || student("Alice") || student("Bob")

  def main(args: Array[String]) = system()
}
