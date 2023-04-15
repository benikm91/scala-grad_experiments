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
import scalagrad.api.Deriver
import scalagrad.api.ScalaGrad
import scalagrad.api.test.DeriveSpireNumericDoubleTests
import scalagrad.forward.dual.DualNumber
import scalagrad.forward.DeriverSpireNumericForward

import spire.math.Numeric
import spire.implicits.*

class DeriverForwardSpireNumericDoubleTests extends DeriveSpireNumericDoubleTests("forward-mode") {

  override type DNum[P] = DualNumber[P]
  override val spireNumericDNum: Numeric[DualNumber[Double]] = summon[Numeric[DualNumber[Double]]]
  override val deriver: DoubleDeriver = DeriverSpireNumericForward.spireNumeric[Double]

}

