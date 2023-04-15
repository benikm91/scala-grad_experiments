package scalagrad.test.util

import org.scalacheck.Gen

object TestUtil:

    def approx(f: Double => Double, x: Double, e: Double = 1e-6): Double = (f(x + e) - f(x)) / e

    def approx2(f: (Double, Double) => Double, x1: Double, x2: Double, e: Double = 1e-6): (Double, Double) = (
        (f(x1 + e, x2) - f(x1, x2)) / e, 
        (f(x1, x2 + e) - f(x1, x2)) / e
    )

    val (min, max) = (1e-3, 1e+3)
    def reasonableDoubleGenerator: Gen[Double] = 
      Gen.oneOf(
        Gen.choose(min, max),
        Gen.choose(-max, -min)
      )
    
    def isReasonableDouble(x: Double): Boolean = 
      def isInRange(x: Double): Boolean = min <= x && x <= max
      isInRange(x) || isInRange(-x)