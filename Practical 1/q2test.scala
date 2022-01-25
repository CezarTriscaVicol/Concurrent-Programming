import io.threadcso._
import Practical._

object q2test{
  val random = scala.util.Random
  
  def main(args: Array[String]) = {
    for(i <- 0 until 1000){
      val input0 , input1 , input2 , input3 =OneOne[Int];
      val output0, output1, output2, output3=OneOne[Int];
      var y0, y1, y2, y3: Int=0;
      run(proc{ input0!random.nextInt(1000); input0.closeIn } ||
          proc{ input1!random.nextInt(1000); input1.closeIn  } ||
          proc{ input2!random.nextInt(1000); input2.closeIn  } ||
          proc{ input3!random.nextInt(1000); input3.closeIn  } ||
          sort4((input0  :: input1  :: input2  :: input3  :: Nil),
                (output0 :: output1 :: output2 :: output3 :: Nil)) ||
          proc{ y0 = output0?() } ||
          proc{ y1 = output1?() } ||
          proc{ y2 = output2?() } ||
          proc{ y3 = output3?() } )
          //println(y0 + " " + y1 + " " + y2 + " " + y3)
          assert( y0 <= y1 ); assert( y0 <= y2 ); assert( y0 <= y3 );
          assert( y1 <= y2 ); assert( y1 <= y3 );
          assert( y2 <= y3 );
          print('.')
    }
    io.threadcso.exit()
  }
}
