import io.threadcso._
import scala.language.postfixOps

/** Simulation of the Dining Philosophers example. */
object RightButlerTimeoutPhils{
  val N = 5 // Number of philosophers

  var rightHand, withButler, timeout = false; 

  // Simulate basic actions
  def Eat = Thread.sleep(50)
  def Think = Thread.sleep(scala.util.Random.nextInt(90))
  def Pause = Thread.sleep(50)

  // Each philosopher will send "pick" and "drop" commands to her forks, which
  // we simulate using the following values.
  type forkChannel = channel.DeadlineManyOne[Command]
  type Command = Boolean
  val Pick = true; val Drop = false
  type Request = Int
  val Sit  = -1; val Leave = 1

  /** The butler seating philosophers. */
  def butler(sit: Chan[Request], leave: Chan[Request]) = proc("Butler"){
    val Sitting  = -1; val Standing = 1
    val seated = Array.fill(6)(1)
    var seatedCount: Int = 0
    serve(
      ((seatedCount < 4) && sit) =?=> {
        x => 
        assert(-5 <= x && x < 0)
        assert(seated(x*(-1)) == Standing)
        seatedCount += seated(x*(-1))
        seated(x*(-1)) = Sitting
      }
      |
      leave =?=> {
        x => assert(0 < x && x <= 5)
        assert(seated(x) == Sitting)
        seatedCount += seated(x)
        seated(x) = Standing
      }
    )
  }
 
  /** A single philosopher that could be left-handed or right-handed. */
  def phil(me: Int, leftPick: forkChannel, leftDrop: forkChannel, rightPick: forkChannel, rightDrop: forkChannel, 
                    toButlerSit: ![Request], toButlerLeave: ![Request]) = proc("Phil"+me){
    repeat{
      Think
      if(withButler) 
      	toButlerSit!(Sit*(me+1))
      println(me+" sits"); Pause
      if(timeout){  
        var hasEaten = false
        while(!hasEaten){
          if(me == 0 && rightHand){
            rightPick!Pick; println(me+" picks up right fork"); Pause
            if(leftPick.writeBefore(10000000)(Pick)){
              println(me+" picks up left fork"); Pause
              println(me+" eats"); Eat
              leftDrop!Drop; Pause; rightDrop!Drop; Pause
              hasEaten = true
            }else{
              rightDrop!Drop; println(me+" puts down right fork"); Pause
              Think
            }
          }else{ 
            leftPick!Pick; println(me+" picks up left fork"); Pause
            if(rightPick.writeBefore(10000000)(Pick)){
              println(me+" picks up right fork"); Pause
              println(me+" eats"); Eat
              leftDrop!Drop; Pause; rightDrop!Drop; Pause
              hasEaten = true
            }else{
              leftDrop!Drop; println(me+" puts down left fork"); Pause
              Think
            }
          }
        }
      }else{
        if(me == 0 && rightHand){
          rightPick!Pick; println(me+" picks up right fork"); Pause
        	leftPick!Pick;  println(me+" picks up left fork"); Pause
        }else{
        	leftPick!Pick;  println(me+" picks up left fork"); Pause
     	    rightPick!Pick; println(me+" picks up right fork"); Pause
        }
        println(me+" eats"); Eat
        leftDrop!Drop; Pause; rightDrop!Drop; Pause
      }
      if(withButler) 
     	 toButlerLeave!(Leave*(me+1))
      println(me+" leaves")
    }
  }

  /** A single fork. */
  def fork(me: Int, fromPhilPick: forkChannel, fromPhilDrop: forkChannel) = proc("Fork"+me){
    var onTable: Boolean = true
    repeat{
      if(onTable){
        val x = fromPhilPick?()
        assert(x == Pick)
        onTable = false
      }else{
        val x = fromPhilDrop?()
        assert(x == Drop)
        onTable = true
      }
    }
  }

  /** The complete system. */
  def system = {
    // Channels to pick up and drop the forks:
    val philToForkPick, philToForkDrop = Array.fill(5)(new forkChannel)
    // Channels for the philosophers to ask the butler to be seated:
    val philToButlerSit, philToButlerLeave = ManyOne[Request]
    // We have the following order: fork(i), phil(i), fork(i+1)
    val allPhils = || ( 
      for (i <- 0 until N)
      yield phil(i, philToForkPick(i), philToForkDrop(i), philToForkPick((i+1)%N), philToForkDrop((i+1)%N), philToButlerSit, philToButlerLeave)
    )
    val allForks = || ( 
      for (i <- 0 until N) yield
        fork(i, philToForkPick(i), philToForkDrop(i))
    )
    if(withButler)
    	allPhils || allForks || butler(philToButlerSit, philToButlerLeave)
    else
    	allPhils || allForks 
  }

  /** Run the system. */
  def main(args : Array[String]) = { 
    var i = 0
    while(i < args.length) args(i) match{
      case "-righthand" => rightHand  = true; i += 1
      case "-butler"    => withButler = true; i += 1
      case "-timeout"   => timeout    = true; i += 1
      case arg => println("Unrecognised argument: "+arg); sys.exit
    }
    system() 
  }
}

  
