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

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("Bogus5") = forAll(genNonEmptyHeap, genNonEmptyHeap) { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    val min = if (h1Min < h2Min) h1Min else h2Min

    val h3 = meld(h1, h2)
    findMin(h3) == min
  }
}
