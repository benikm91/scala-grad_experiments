package scalagrad.fractional.auto.forward

import scalagrad.deriver.test.DoubleDoubleBasicTests
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.DeriverForwardPlan
import scalagrad.auto.forward.DeriverForwardPlan.given
import scalagrad.fractional.auto.dual.DualIsFractional.given

import scalagrad.fractional.auto.dual.DualIsFractional

import scalagrad.auto.forward.DeriverForwardPlan

class DeriverForwardFractionalDoubleDoubleTests extends DoubleDoubleBasicTests("forward-mode") {

  override type T = DualNumber[Double]
  override val fractionalDNum: Fractional[DualNumber[Double]] = summon[Fractional[DualNumber[Double]]]
  override val deriver: DoubleDoubleDeriver = DeriverForwardPlan.tuple2

}

