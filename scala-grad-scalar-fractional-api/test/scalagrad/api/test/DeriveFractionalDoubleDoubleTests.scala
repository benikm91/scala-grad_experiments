package scalagrad.api.test

import collection.mutable.Stack
import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink

import scalagrad.api.Deriver
import scalagrad.api.ScalaGrad

abstract class DeriveFractionalDoubleDoubleTests(val name: String) extends AnyWordSpec with should.Matchers:

  type DNum[P]
  given fractionalDNum: Fractional[DNum[Double]]
  type DoubleDoubleDeriver = Deriver[(DNum[Double], DNum[Double]) => DNum[Double]] {
    type dfInput = (Double, Double)
    type dfOutput = (Double, Double)
  }
  val deriver: DoubleDoubleDeriver

  def generateDoubleInReasonableRange(min: Double, max: Double): Gen[Double] =
    def generatePositiveDoubleInReasonableRange: Gen[Double] = Gen.choose(min, max)
    def generateNegativeDoubleInReasonableRange: Gen[Double] = generatePositiveDoubleInReasonableRange.map(x => -x)
      Gen.oneOf(
        generatePositiveDoubleInReasonableRange,
        generateNegativeDoubleInReasonableRange
      )
    
  /**
   * Tests calculation of derivative for f by comparing it to its approximation.
   * As the approximation can not be made arbitrary precise, generated numbers are restricted.
   *
   * @param f Binary Function to calculate and test derivatives for.
   * @param e Precision of approximation. If to small approximation will become 0.0 due to floating point imprecision.
   * @param min Minimum value of generated numbers. If to small approximation will become 0.0 due to floating point imprecision.
   * @param max Maximum value of generated numbers. If to big precision of approximation will hurt threshold.
   * @param tolerance When comparing calculated derivative and approximated derivative, this tolerance is used.
   */
  def testApproximation(
    f: [T] => (T, T) => Fractional[T] ?=> T,
    e: Double = 1e-6,
    min: Double = 1e-3,
    max: Double = 1e+3,
    tolerance: Double = 1e-2,
  ): Unit =
    forAll(generateDoubleInReasonableRange(min, max), generateDoubleInReasonableRange(min, max)) { (x1: Double, x2: Double) =>
      whenever(
        min <= x1.abs && x1.abs <= max &&
        min <= x2.abs && x2.abs <= max
      ) {
        val approxDx1: Double = (f(x1 + e, x2) - f(x1, x2)) / e
        val approxDx2: Double = (f(x1, x2 + e) - f(x1, x2)) / e
        val df = ScalaGrad.derive(f[DNum[Double]])(using deriver)
        val (dx1: Double, dx2: Double) = df(x1, x2)
        dx1 should be(approxDx1 +- tolerance)
        dx2 should be(approxDx2 +- tolerance)
      }
    }

  f"${name} deriviation" should {
    "work for addition" in {
      testApproximation([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 + x2
      )
    }
    "work for subtraction" in {
      testApproximation([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 - x2
      )
    }
    "work for multiply" in {
      testApproximation([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 * x2
      )
    }
    "work for division" in {
      testApproximation([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 / x2
      )
    }
    "work with a mixture of all basic operations" in {
      testApproximation([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        (x1 + x2) * (x2 / x1) + x1 - x2 * (x2 + x1 - x1 * x2) / x2
      )
    }
  }
