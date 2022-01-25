import io.threadcso._

object Practical{
  /** A single comparator, inputting on in0 and in1, and outputting on out0
  * (smaller value) and out1 (larger value). */
  def comparator(in0: ?[Int], in1: ?[Int], out0: ![Int], out1: ![Int]): PROC = proc {
    repeat{
      var a, b: Int =0;
      run(proc{a=in0?()} || proc{b=in1?()})
      if(a > b){
        run(proc{out0!b} || proc{out1!a})
      }else{ // a < b
        run(proc{out0!a} || proc{out1!b})
      }
    }
    out0.closeOut; out1.closeOut
  }
  
  /** A sorting network for four values. */
  def sort4(ins: List [?[Int ]], outs: List [![ Int ]]): PROC = proc {
    require(ins.length == 4 && outs.length == 4)
    val x00=ins.head; val x10=(ins.tail).head;
    val x20=((ins.tail).tail).head; val x30=(((ins.tail).tail).tail).head;
    val x01=OneOne[Int];
    val x11, x12=OneOne[Int];
    val x21, x22=OneOne[Int];
    val x31=OneOne[Int];
    val y0=outs.head; val y1=(outs.tail).head;
    val y2=((outs.tail).tail).head; val y3=(((outs.tail).tail).tail).head;
    run( comparator(x00, x20, x01, x21) ||
         comparator(x10, x30, x11, x31) ||
         comparator(x01, x11, y0 , x12) ||
         comparator(x21, x31, x22, y3 ) ||
         comparator(x12, x22, y1 , y2 ) )
    y0.closeOut
    y1.closeOut
    y2.closeOut
    y3.closeOut
  }

  def insert(ins: List [?[Int ]], in: ?[Int ], outs: List [![ Int ]]): PROC = proc {
    val n = ins.length; require(n >= 1 && outs.length == n+1)
      if(n == 1){
        val x0=outs.head
        val x1=(outs.tail).head
        run(comparator(ins.head, in, x0, x1))
      }else{
        val ins0=OneOne[Int]
        run(comparator(ins.head, in, outs.head, ins0) ||
                insert(ins.tail, ins0, outs.tail))
      }
      var aux_outs = outs
      for(_ <- 1 to n){
        aux_outs.head.closeOut
        aux_outs = aux_outs.tail
      }
  }

  def insertion_sort(ins: List [?[Int ]], outs: List [![ Int ]]): PROC = proc {
    val n = ins.length; require(n >= 2 && outs.length == n)
    if(n == 2){
      run(comparator(ins.head, (ins.tail).head, outs.head, (outs.tail).head))
    }else{
      var inter_list:List[Chan[Int]] = Nil
      for(j <- 2 to n){
        val inter_channel = OneOne[Int]
        inter_list = inter_channel :: inter_list
      } 
      run(insertion_sort(ins.tail, inter_list) ||
          insert(inter_list, ins.head, outs))
      for(j <- 2 to n){
        inter_list.head.close
        inter_list = inter_list.tail
      }
    }
    var aux_outs = outs
    for(_ <- 1 to n){
      aux_outs.head.closeOut
      aux_outs = aux_outs.tail
    }
  }
}
