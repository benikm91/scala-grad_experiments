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

import spire.math.Numeric
import spire.implicits.*

abstract class DeriveSpireNumericDoubleTests(val name: String) extends AnyWordSpec with should.Matchers:

  type DNum[P]
  given spireNumericDNum: Numeric[DNum[Double]]
  type DoubleDeriver = Deriver[(DNum[Double]) => DNum[Double]] {
    type dfInput = Double
    type dfOutput = Double
  }
  val deriver: DoubleDeriver

  def generatePositiveDoubleInReasonableRange(min: Double, max: Double): Gen[Double] = Gen.choose(min, max)
  def generateNegativeDoubleInReasonableRange(min: Double, max: Double): Gen[Double] = generatePositiveDoubleInReasonableRange(min, max).map(x => -x)
  def generateDoubleInReasonableRange(min: Double, max: Double): Gen[Double] =
      Gen.oneOf(
          generatePositiveDoubleInReasonableRange(min, max),
          generateNegativeDoubleInReasonableRange(min, max)
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
    f: [T] => (T) => Numeric[T] ?=> T,
    generator: Gen[Double],
    isLegalGeneratorValue: Double => Boolean,
    e: Double = 1e-6,
    tolerance: Double = 1e-2,
  ): Unit =
    forAll(generator) { (x: Double) =>
      whenever(
        isLegalGeneratorValue(x)
      ) {
        val approxDx: Double = (f(x + e) - f(x)) / e
        val df = ScalaGrad.derive(f[DNum[Double]])(using deriver)
        val dx = df(x)
        dx should be(approxDx +- tolerance)
      }
    }

  val min: Double = 1e-3
  val max: Double = 1e+3
  def isInRange(x: Double): Boolean = min <= x && x <= max
  
  f"${name} deriviation" should {
    "work for nroot" in {
        testApproximation([T] => (x: T) => (num: Numeric[T]) ?=> 
          num.nroot(x, 2),
        generator = generatePositiveDoubleInReasonableRange(min, max),
        isLegalGeneratorValue = (x: Double) => isInRange(x),
      )
    }
  }
