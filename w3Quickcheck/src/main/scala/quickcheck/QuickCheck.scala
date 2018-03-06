package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    arb <- arbitrary[Int]
    oof <- oneOf[H](const[H](empty), genHeap)
  } yield insert(arb, oof)

  lazy val genNonEmptyHeap: Gen[H] = for {
    h <- arbitrary[H]
    x <- arbitrary[Int]
  } yield if (isEmpty(h)) insert(x, h) else h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("Bogus3") = forAll { (a: Int, b: Int) =>
    val min = if (a < b) a else b
    val max = if (a > b) a else b
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val h3 = deleteMin(h2)
    findMin(h3) == max
  }

  property("Bogus4") = forAll { (xs: List[Int]) =>
    def insertAll(xs: List[Int], heap: H): H = xs match {
      case Nil => empty
      case y :: ys =>
        insert(y, insertAll(ys, heap))
    }

    def removeAll(heap: H): List[Int] =
      if (isEmpty(heap)) Nil
      else {
        val min = findMin(heap)
        val h = deleteMin(heap)
        min :: removeAll(h)
      }

    val h = insertAll(xs, empty)
    val ys = removeAll(h)
    xs.sorted == ys
  }

  property("Bogus5") = forAll(genNonEmptyHeap, genNonEmptyHeap) { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    val min = if (h1Min < h2Min) h1Min else h2Min

    val h3 = meld(h1, h2)
    findMin(h3) == min
  }
}
