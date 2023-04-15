package asdsada

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
import scalagrad.api.test.DeriveFractionalDoubleDoubleTests
import scalagrad.forward.dual.DualNumber
import scalagrad.forward.DeriverFractionalForward

class DeriverForwardFractionalDoubleDoubleTests extends DeriveFractionalDoubleDoubleTests("forward-mode") {

  override type DNum[P] = DualNumber[P]
  override val fractionalDNum: Fractional[DualNumber[Double]] = summon[Fractional[DualNumber[Double]]]
  override val deriver: DoubleDoubleDeriver = DeriverFractionalForward.fractional2[Double]

}

