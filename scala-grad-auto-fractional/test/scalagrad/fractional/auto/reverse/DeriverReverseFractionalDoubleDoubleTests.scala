package scalagrad.fractional.auto.reverse

import collection.mutable.Stack
import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import scala.math.Fractional
import scalagrad.api.Deriver
import scalagrad.api.ScalaGrad
import scalagrad.deriver.test.DoubleDoubleBasicTests
import scalagrad.auto.reverse.dual.DualDelta
import scalagrad.auto.reverse.DeriverReversePlan.DeriverReversePlanDouble
import scalagrad.fractional.auto.dual.DualIsFractional.given

import scalagrad.fractional.auto.dual.DualIsFractional
class DeriverReverseFractionalDoubleDoubleTests extends DoubleDoubleBasicTests("reverse-mode") {

  override type T = DualDelta[Double]
  override val fractionalDNum: Fractional[DualDelta[Double]] = summon[Fractional[DualDelta[Double]]]
  override val deriver: DoubleDoubleDeriver = DeriverReversePlanDouble.tuple2

}

