import scala.util.Random
import io.threadcso._

abstract class IntTree
case class Leaf(n: Int) extends IntTree
case class Branch(l: IntTree, r: IntTree) extends IntTree

object TreeSumTest{
  /** Produce a random tree.
    * @param w the reciprocal of the probability of producing a Leaf. */
  def makeTree(w: Int): IntTree = {
    if(Random.nextInt(w) == 0) return new Leaf(Random.nextInt(100))
    else return new Branch(makeTree(w-1), makeTree(w-1))
  }

  /** Sequential tree sum. */
  def treeSum(t: IntTree): Int = t match{
    case Leaf(n) => n
    case Branch(l, r) => treeSum(l) + treeSum(r)
  }

  def thread(t: IntTree, returnChan: ![Int]): PROC = proc("Thread"+t){
    returnChan!(t match{
      case Leaf(n) => n
      case Branch(l, r) => {
        val chanLeft, chanRight = OneOne[Int]
        var aux = 0
        run(thread(l, chanLeft) || thread(r, chanRight) || proc{ aux = chanLeft?() + chanRight?() })
        chanLeft.close; chanRight.close
        aux
      }
    })
  }

  /** Concurrent tree sum. */
  def TreeSum(t: IntTree): Int = {
    val returnChan = OneOne[Int]
    var aux = 0
    run(thread(t, returnChan) || proc{ aux = returnChan?() })
    returnChan.close
    aux
  }

  /** A single test. */
  def doTest = {
    val t = makeTree(4); val seqSum = treeSum(t); val concSum = TreeSum(t)
    assert(seqSum == concSum)
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 100000){
      doTest; if(i%1000 == 0) print(".")
    }
    println; io.threadcso.exit
  }
}
