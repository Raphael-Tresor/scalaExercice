package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.{forAll, forAllNoShrink}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =
    oneOf(const(empty),
      for {
        i <- Arbitrary.arbitrary[Int]
        heap <- genHeap
      } yield insert(i, heap)
    )

  lazy val genInt: Gen[Int] = Arbitrary.arbitrary[Int]

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("give me back my insertion") = forAll { (n:Int) =>
    findMin(insert(n,empty)) == n
  }

  property("min of two Int is min of the Heap in which they were Inserted") = forAll { (a: Int, b: Int) =>
    findMin(insert(a,insert(b,empty))) == a.min(b)
  }

  property("delete min does not disturbe the ranking") = forAll { (a: Int, b: Int, c:Int) =>
    val heap = insert(a,insert(b,insert(c,empty)))
    val l: List[Int] = List(a,b,c).sorted.tail
    drain(deleteMin(heap),Nil) == l
  }
  property("drainedHeap") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a,empty))) == true
  }

  property("minOfmeldingHeaps") = forAll { (h1: H, h2: H) =>
    if isEmpty(h1) && isEmpty(h2) then true
    else if isEmpty(h1) && !isEmpty(h2) then findMin(h2) == findMin(meld(h1,h2))
    else if !isEmpty(h1) && isEmpty(h2) then findMin(h1) == findMin(meld(h1,h2)) else
      findMin(h1).min(findMin(h2)) == findMin(meld(h1,h2))
  }
  @annotation.tailrec
  final private def drain( h: H, acc: List[A])(implicit ord: Ordering[A]): List[A] =
    isEmpty(h) match
      case true => acc.reverse
      case _ => drain(deleteMin(h), findMin(h) :: acc)(ord)

  property("draining Heap is sorting its elements") = forAll{ (h:H) =>
    val sortedElements = drain(h,Nil)
    sortedElements == sortedElements.sorted(Ordering.Int)
  }

