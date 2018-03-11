package calculator

import scala.math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      val lA = a()
      val lB = b()
      val lC = c()
      //      Δ = b² - 4ac
      lB * lB - 4 * lA * lC
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      // (-b ± √Δ) / 2a
      delta match {
        case dt if dt() > 0 => {
          Set(
            (-b() + sqrt(dt())) / (2 * a()),
            (-b() - sqrt(dt())) / (2 * a())
          )
        }
        case dt if dt() == 0 => Set(-b() / (2 * a()))
        case dt if dt() < 0 => Set(0)
      }
    }
  }
}
