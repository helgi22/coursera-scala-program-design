import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

val propConcatList = forAll { (l1: List[Int], l2: List[Int]) => l1.size + l2.size == (l1 ::: l2).size }

propConcatList.check

val f: ((Int) => Boolean) = (n) => scala.math.sqrt(n * n) == n

val propSqrt = forAll(f)

propSqrt.check
