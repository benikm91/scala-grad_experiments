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
import spire.algebra.Trig

abstract class DeriveSpireTrigDoubleTests(val name: String) extends AnyWordSpec with should.Matchers:

  type DNum[P]
  given spireNumericDNum: Numeric[DNum[Double]]
  given spireTrigDNum: Trig[DNum[Double]]
  type DoubleDeriver = Deriver[(DNum[Double]) => DNum[Double]] {
    type dfT = Double => Double
  }
  val deriver: DoubleDeriver

  def testF(
    f: [T] => (x: T) => (trig: Trig[T]) ?=> T, 
    min: Double = 1e-3, 
    max: Double = 1e+3, 
    tolerance : Double = 1e-2
  ) = 
    forAll(Gen.choose(min, max)) { (x: Double) =>
      whenever(min <= x && x <= max) {
        val approxDx: Double = ScalaGrad.derive(f[Double])(using approx(1e-6))(x)
        val df = ScalaGrad.derive(f[DNum[Double]])(using deriver)
        val dx = df(x)
        dx should be(approxDx +- tolerance)
      }
    }

  def testC(
    f: [T] => (x: T) => (trig: Trig[T], num: Numeric[T]) ?=> T, 
    min: Double = 1e-3, 
    max: Double = 1e+3, 
    tolerance : Double = 1e-2
  ) = 
    forAll(Gen.choose(min, max)) { (x: Double) =>
      whenever(min <= x && x <= max) {
        val approxDx: Double = ScalaGrad.derive(f[Double])(using approx(1e-6))(x)
        val df = ScalaGrad.derive(f[DNum[Double]])(using deriver)
        val dx = df(x)
        dx should be(approxDx +- tolerance)
      }
    }

  f"${name} basic deriviation" should {
    "work for exp" in {
      testF(
        [T] => (x: T) => (trig: Trig[T]) ?=> trig.exp(x), 
        // exp grows to fast for default min/max
        min = -5.0,
        max = 5.0
      )
    }
    "work for expm1" in {
      testF(
        [T] => (x: T) => (trig: Trig[T]) ?=> trig.expm1(x),
        // expm1 grows to fast for default min/max
        min = -5.0,
        max = 5.0
      )
    }
    "work for log" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.log(x)
      )
    }
    "work for log1p" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.log1p(x)
      )
    }
    "work for sin" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.sin(x)
      )
    }
    "work for cos" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.cos(x)
      )
    }
    "work for tan" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.tan(x)
      )
    }
    "work for asin" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.asin(x),
        // asin only defined in [-1, 1]
        min = -1.0,
        max = 1.0
      )
    }
    "work for acos" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.acos(x),
        // acos only defined in [-1, 1]
        min = -1.0,
        max = 1.0
      )
    }
    "work for atan" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.atan(x)
      )
    }
    // "work for atan2" in {
    //   testF([T] => (x: T) => (trig: Trig[T]) ?=> 
    //     trig.atan2(x)
    //   )
    // }
    "work for sinh" in {
      testF(
        [T] => (x: T) => (trig: Trig[T]) ?=> trig.sinh(x),
        // sinh grows to fast for default min/max
        min = -10.0,
        max = 10.0
      )
    }
    "work for cosh" in {
      testF(
        [T] => (x: T) => (trig: Trig[T]) ?=> trig.cosh(x),
        // sinh grows to fast for default min/max
        min = -10.0,
        max = 10.0
      )
    }
    "work for tanh" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.tanh(x)
      )
    }
    "work for toRadians" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.toRadians(x)
      )
    }
    "work for toDegrees" in {
      testF([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.toDegrees(x)
      )
    }
  }

  f"${name} chaining deriviation" should {

    def addExtraOps(f: [T] => (x: T) => (trig: Trig[T]) ?=> T): 
      [T] => (x: T) => (trig: Trig[T], num: Numeric[T]) ?=> T =
      [T] => (x: T) => (trig: Trig[T], num: Numeric[T]) ?=> 2 * f(x / 2)

    "work for exp" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.exp(x)),
        // exp grows to fast for default min/max
        min = -5.0,
        max = 5.0
      )
    }
    "work for expm1" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.expm1(x)),
        // expm1 grows to fast for default min/max
        min = -5.0,
        max = 5.0
      )
    }
    "work for log" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.log(x))
      )
    }
    "work for log1p" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.log1p(x))
      )
    }
    "work for sin" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.sin(x))
      )
    }
    "work for cos" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.cos(x))
      )
    }
    "work for tan" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.tan(x))
      )
    }
    "work for asin" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.asin(x)),
        // asin only defined in [-1, 1]
        min = -1.0,
        max = 1.0
      )
    }
    "work for acos" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.acos(x)),
        // acos only defined in [-1, 1]
        min = -1.0,
        max = 1.0
      )
    }
    "work for atan" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> 
        trig.atan(x))
      )
    }
    "work for sinh" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.sinh(x)),
        // sinh grows to fast for default min/max
        min = -10.0,
        max = 10.0
      )
    }
    "work for cosh" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.cosh(x)),
        // sinh grows to fast for default min/max
        min = -10.0,
        max = 10.0
      )
    }
    "work for tanh" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.tanh(x))
      )
    }
    "work for toRadians" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.toRadians(x))
      )
    }
    "work for toDegrees" in {
      testC(
        addExtraOps([T] => (x: T) => (trig: Trig[T]) ?=> trig.toDegrees(x))
      )
    }
  }
