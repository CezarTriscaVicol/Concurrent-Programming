import io.threadcso._

object PipelineSortChannels{
  // PipelineSort: sort data received on in, and output on out
  def pipelineSort(N: Int, in: ?[Int], out: ![Int]): PROC = proc("PIPELINESORT"){

    var global_cnt = 0

    def component(in_c: ?[Int], out_c: ![Int]) = proc("Component"){
      var old_val = -1;
      val component_id = global_cnt;
      global_cnt += 1
      //println("Initialized component: "+component_id)
      repeat{ 
        var new_val = in_c?();
        //println("component: "+component_id+"| "+old_val+", "+new_val)
        if(old_val == -1){
          old_val = new_val
        }else{
          if(old_val < new_val){
            val aux = old_val
            old_val = new_val
            new_val = aux
          }
          //println("pushing component: "+component_id+" -> "+new_val)
          out_c!new_val
        }
      }
      //println("Closing component: "+component_id)
      out_c!old_val
      in_c.closeIn; out_c.closeOut
    }

    // Put the system together, and run it
    var current_channel = OneOne[Int]
    var sequence:Seq[PROC] = Seq(component(in, current_channel))
    for(i <- 1 until N){
      val new_channel = OneOne[Int] 
      sequence = sequence :+ component(current_channel, new_channel)
      current_channel = new_channel
    }
    sequence = sequence :+ component(current_channel, out)
    par(sequence)
  }
}

// =======================================================

import scala.util.Random

object PipeSortChannelsTest{
  // Number of elements to sort; range of input values.
  val N = 50; val Max = 500

  /** Run a single test.  Generate N random numbers.  Pass them in to a sorter.
    * Receive outputs.  Check result is as expected. */
  def doTest = {
    val xs = Array.fill(N)(Random.nextInt(Max))
    val ys = new Array[Int](N)
    val in, out = OneOne[Int]
    def sender = proc{ for(x <- xs) in!x; in.close }
    def receiver = proc{ var i = 0; repeat{ ys(i) = out?(); i += 1 } }
    run(sender || PipelineSortChannels.pipelineSort(N, in, out) || receiver)
    assert(xs.sorted.sameElements(ys))
  }

  def main(args : Array[String]) = {
    val start = System.nanoTime
    for(i <- 0 until 1000){ doTest; if(i%10 == 0) print(".") }
    val duration = System.nanoTime - start
    val avg_duration = duration / 1000
    println
    println(avg_duration)
    exit
  }   
}
