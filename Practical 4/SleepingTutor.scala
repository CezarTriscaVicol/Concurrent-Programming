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

class SleepingTutorMonitor extends SleepingTutor{
  var tuteDone: Int = 0
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
    while(tuteDone == 0) wait() // wait until tutorial is done
    count -= 1
    tuteDone -= 1
    notifyAll() // notify tutor that student left
  }

  /** A tutor ends the tutorial. */
  def endTeach = synchronized {
    tuteDone = 2 // signal to students that tutorial is done
    notifyAll()
    while(tuteDone > 0) wait() // wait for students to leave
  }
}

// =======================================================

class BooleanSemaphore(available: Boolean){
  private var isUp = available // Is the flag up?

  def down = synchronized{
    while(!isUp) wait()
    isUp = false
  }

  def up = synchronized{
    isUp = true
    notify()
  }
}

class SleepingTutorSemaphore extends SleepingTutor{
  private val mutex = BooleanSemaphore(true)
  private val tutorSleep = BooleanSemaphore(false)
  private val firstSleep = BooleanSemaphore(false)
  private val studentSleep = BooleanSemaphore(false)
  private var count = 0
  /** A tutor waits for students to arrive. */
  def tutorWait = {
    tutorSleep.down 
    assert(count == 2)
  }

  /** A student arrives and waits for the tutorial. */
  def arrive = {
    mutex.down
    count += 1
    if(count == 1) {
      mutex.up
      firstSleep.down // wait for signal from the second student
      tutorSleep.up   // pass the baton to tutor
    }
    else { // count == 2, we're the second student to arrive
      assert(count == 2)
      mutex.up
      firstSleep.up  // pass the baton to first student
    }    
  }
  
  /** A student receives a tutorial. */
  def receiveTute = {
    studentSleep.down // wait to be awoken by student/teacher
    count -= 1
    if(count == 1)
      studentSleep.up // awake the other student
  }

  /** A tutor ends the tutorial. */
  def endTeach = {
    studentSleep.up // awake one student
  }
}

// =======================================================

import scala.util.Random

object SleepingTutorSimulation{
  private var st: SleepingTutor = new SleepingTutorMonitor

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

  def main(args: Array[String]) = {
    if(args(0) == "-semaphore") st = new SleepingTutorSemaphore
    system()
  }
}
