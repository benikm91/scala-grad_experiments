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
import scalagrad.spire.auto.dual.DualIsNumeric.given
import scalagrad.spire.auto.test.DeriveSpireTrigDoubleTests

import scalagrad.auto.forward.dual.DualNumber

import spire.math.Numeric
import spire.implicits.*
import scalagrad.auto.forward.DeriverForwardPlan
import scalagrad.auto.forward.DeriverForwardPlan.given

import scalagrad.auto.forward.DeriverForwardPlan
import spire.algebra.Trig

class DeriverForwardSpireTrigDoubleTests extends DeriveSpireTrigDoubleTests("forward-mode") {

  override type DNum[P] = DualNumber[P]
  override val spireNumericDNum: Numeric[DNum[Double]] = summon[Numeric[DualNumber[Double]]]
  override val spireTrigDNum: Trig[DualNumber[Double]] = summon[Trig[DualNumber[Double]]]
  override val deriver: DoubleDeriver = DeriverForwardPlan.single

}

