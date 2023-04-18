package scalagrad.fractional.auto.forward

import scalagrad.deriver.test.DoubleDoubleBasicTests
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.DeriverForwardPlan.DeriverForwardPlanDouble
import scalagrad.fractional.auto.forward.dual.DualNumberIsFractional.given

class DeriverForwardFractionalDoubleDoubleTests extends DoubleDoubleBasicTests("forward-mode") {

  override type T = DualNumber[Double]
  override val fractionalDNum: Fractional[DualNumber[Double]] = summon[Fractional[DualNumber[Double]]]
  override val deriver: DoubleDoubleDeriver = DeriverForwardPlanDouble.tuple2

}

