package scalagrad.deriver.test

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

import scalagrad.test.util.TestUtil.{reasonableDoubleGenerator, isReasonableDouble}
import scalagrad.fractional.numerical.DeriverNumerical.approx2

abstract class DoubleDoubleBasicTests(val name: String) extends AnyWordSpec with should.Matchers:

  type T
  given fractionalDNum: Fractional[T]
  type DoubleDoubleDeriver = Deriver[(T, T) => T] {
    type dfInput = (Double, Double)
    type dfOutput = (Double, Double)
  }
  val deriver: DoubleDoubleDeriver 

  def testF(f: [T] => (x1: T, x2: T) => (num: Fractional[T]) ?=> T) = 
    val tolerance = 1e-2
    forAll(reasonableDoubleGenerator, reasonableDoubleGenerator) { (x1: Double, x2: Double) =>
      whenever(isReasonableDouble(x1) && isReasonableDouble(x2)) {
        val df = ScalaGrad.derive(f[T])(using deriver)
        val (dx1, dx2) = df(x1, x2)
        val (approxDx1, approxDx2) = ScalaGrad.derive(f[Double])(using approx2(1e-6))(x1, x2)
        dx1 should be(approxDx1 +- tolerance)
        dx2 should be(approxDx2 +- tolerance)
      }
    }

  f"${name} deriviation" should {
    "work for addition" in {
      testF([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 + x2
      )
    }
    "work for subtraction" in {
      testF([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 - x2
      )
    }
    "work for multiply" in {
      testF([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 * x2
      )
    }
    "work for division" in {
      testF([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        x1 / x2
      )
    }
    "work with a mixture of all operations" in {
      testF([T] => (x1: T, x2: T) => (f: Fractional[T]) ?=> 
        import f.*
        (x1 + x2) * (x2 / x1) + x1 - x2 * (x2 + x1 - x1 * x2) / x2
      )
    }
  }
