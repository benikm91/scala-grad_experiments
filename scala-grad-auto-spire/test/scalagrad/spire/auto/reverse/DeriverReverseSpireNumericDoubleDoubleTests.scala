package scalagrad.spire.auto.forward.test

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
import scalagrad.auto.reverse.dual.DualDelta
import scalagrad.spire.auto.dual.DualIsNumeric.given
import scalagrad.spire.auto.test.DeriveSpireNumericDoubleDoubleTests

import spire.math.Numeric
import spire.implicits.*
import scalagrad.auto.reverse.DeriverReversePlan
import scalagrad.auto.reverse.DeriverReversePlan.given

class DeriverReverseSpireNumericDoubleDoubleTests extends DeriveSpireNumericDoubleDoubleTests("reverse-mode") {

  override type DNum[P] = DualDelta[P]
  override val spireNumericDNum: Numeric[DualDelta[Double]] = summon[Numeric[DualDelta[Double]]]
  override val deriver: DoubleDoubleDeriver = DeriverReversePlan.tuple2

}

