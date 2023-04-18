package scalagrad.fractional.auto.reverse.test

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
import scalagrad.fractional.api.test.DeriveFractionalDoubleDoubleTests
import scalagrad.fractional.auto.reverse.dual.DualDelta
import scalagrad.fractional.auto.reverse.DeriverFractionalReverse

class DeriverReverseFractionalDoubleDoubleTests extends DeriveFractionalDoubleDoubleTests("reverse-mode") {

  override type DNum[P] = DualDelta[P]
  override val fractionalDNum: Fractional[DualDelta[Double]] = summon[Fractional[DualDelta[Double]]]
  override val deriver: DoubleDoubleDeriver = DeriverFractionalReverse.fractional2[Double]

}

