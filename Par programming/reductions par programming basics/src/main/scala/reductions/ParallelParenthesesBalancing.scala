package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i = 0
    var countSoFar = 0
    while (i < chars.length&&countSoFar>=0) {
      countSoFar+=(if (chars(i) == '(') 1 else if (chars(i) == ')') -1 else 0)
      i+=1
    }
    countSoFar==0
  }

    // def go(chs:Array[Char],acc:Int):Boolean =
//        if (chs.length>0) {
//          val countSoFar = acc + (if (chs(0) == '(') 1 else if (chs(0) == ')') -1 else 0)
//          if (countSoFar < 0) false
//          else go(chs.tail, countSoFar)
//        }
//          else acc==0
//
//    go(chars,0)
//  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, acc: Int, min: Int):(Int,Int) = {
      if (until>idx) {
        val countSoFar = acc + (if (chars(idx) == '(') 1 else if (chars(idx) == ')') -1 else 0)
        traverse(idx+1,until, countSoFar,math.min(countSoFar,min))
      }
      else (acc,min)
    }

    def reduce(from: Int, until: Int): (Int,Int) = {
      val middle = from + (until - from) / 2
      val (rl, rr) = parallel(traverse(from, middle, 0, 0), traverse(middle, until, 0, 0))
      (rl._1+rr._1,math.min(rl._2,rr._2))
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
