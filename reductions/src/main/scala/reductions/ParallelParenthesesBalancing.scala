package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    @annotation.tailrec
    def balanceRec(index: Int, acc:Int): Boolean =
      if chars.length == index then
        acc match
          case 0  => true
          case _ => false
      else (chars(index), acc) match
        case (_, n) if n < 0 => false
        case ('(', _) => balanceRec(index+1, acc + 1)
        case (')', _) => balanceRec(index+1, acc - 1)
        case (_, _) => balanceRec(index+1, acc)
    balanceRec(0, 0)




  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =
    /** arg1 count the unopen  parenthesis and arg2 the unclosed parenthesis
    */
    @annotation.tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int,Int)= {
      if until == idx then (arg1,arg2) else
        (chars(idx),arg1,arg2) match
            case (')',_,unclosed) if unclosed > 0  => traverse(idx+ 1,until, arg1 , arg2-1)
            case (')',_,_)   => traverse(idx+ 1,until, arg1 +1 , arg2)
            case ('(',_,_) => traverse(idx+ 1,until, arg1 , arg2+1)
            case (_,_,_) => traverse(idx+ 1,until, arg1 , arg2)
    }

    def reduce(from: Int, until: Int):(Int,Int)  = {
      if until - from <= threshold then traverse(from,until,0,0) else
        val mid = from + (until-from)/2
        val (a,b) = parallel(reduce(from,mid), reduce(mid, until))
        if a._2 > b._1 then (a._1, b._2 + a._2 - b._1  ) else (a._1 - a._2 + b._1, b._2 )
    }

    reduce(0, chars.length) == (0,0)

  // For those who want more:
  // Prove that your reduction operator is associative!

