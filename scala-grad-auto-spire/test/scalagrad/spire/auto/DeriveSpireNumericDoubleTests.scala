package scalagrad.spire.auto.test

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

import scalagrad.test.util.TestUtil.*
import scalagrad.fractional.numerical.DeriverNumerical.*

abstract class DeriveSpireNumericDoubleTests(val name: String) extends AnyWordSpec with should.Matchers:

  type DNum[P]
  given spireNumericDNum: Numeric[DNum[Double]]
  type DoubleDeriver = Deriver[(DNum[Double]) => DNum[Double]] {
    type dfInput = Double
    type dfOutput = Double
  }
  val deriver: DoubleDeriver

  val tolerance = 1e-2

  f"${name} deriviation" should {
    "work for nroot" in {
      val (min, max) = (1e-3, 1e+3)
      forAll(Gen.choose(min, max), Gen.choose(2, 10)) { (x: Double, n: Int) =>
        def f = [T] => (x: T) => (num: Numeric[T]) ?=> num.nroot(x, n)
        whenever(min <= x && x <= max) {
          val approxDx = ScalaGrad.derive(f[Double])(using approx(1e-6))(x)
          val df = ScalaGrad.derive(f[DNum[Double]])(using deriver)
          val dx = df(x)
          dx should be(approxDx +- tolerance)
        }
      }
    }
    "work for negate" in {
      val (min, max) = (1e-3, 1e+3)
      forAll(Gen.choose(min, max)) { (x: Double) =>
        def f = [T] => (x: T) => (num: Numeric[T]) ?=> num.negate(x)
        whenever(min <= x && x <= max) {
          val approxDx: Double = ScalaGrad.derive(f[Double])(using approx(1e-6))(x)
          val df = ScalaGrad.derive(f[DNum[Double]])(using deriver)
          val dx = df(x)
          dx should be(approxDx +- tolerance)
        }
      }
    }
  }
