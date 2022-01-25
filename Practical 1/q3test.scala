import io.threadcso._
import Practical._

object q3test{
  val random = scala.util.Random
  
  def main(args: Array[String]) = {
    for(_ <- 0 until 1000){
      val len:Int = 10 + random.nextInt(20)
      var unsorted_list:List[Chan[Int]] = Nil
      var input_channel_seq, output_channel_seq: Seq[PROC] = Seq()
      var output_channel_list:List[Chan[Int]] = Nil
      var output_array = Array.ofDim[Int](len)
      for(j <- 1 to len){
        val input_channel, output_channel = OneOne[Int]
        input_channel_seq =  input_channel_seq :+ (proc {input_channel!(10 + random.nextInt(1000)); input_channel.close})
        output_channel_seq =  output_channel_seq :+ (proc {output_array(len-j)=output_channel?(); output_channel.close})
        unsorted_list = input_channel :: unsorted_list
        output_channel_list = output_channel :: output_channel_list
      }
      par(input_channel_seq ++ output_channel_seq :+ insertion_sort(unsorted_list, output_channel_list))
      for(j <- 0 until len-1){
        assert(output_array(j) <= output_array(j+1))
      }
      print('.')
    }
    io.threadcso.exit()
  }
}
