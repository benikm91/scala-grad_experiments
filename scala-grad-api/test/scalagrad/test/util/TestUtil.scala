package scalagrad.test.util

import org.scalacheck.Gen

object TestUtil:

    val (min, max) = (1e-3, 1e+3)
    def reasonableDoubleGenerator: Gen[Double] = 
      Gen.oneOf(
        Gen.choose(min, max),
        Gen.choose(-max, -min)
      )
    
    def isReasonableDouble(x: Double): Boolean = 
      def isInRange(x: Double): Boolean = min <= x && x <= max
      isInRange(x) || isInRange(-x)