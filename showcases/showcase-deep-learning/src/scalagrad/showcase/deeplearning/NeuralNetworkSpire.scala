package scalagrad.showcase.deeplearning

import scala.io.Source
import scalagrad.api.ScalaGrad
import scalagrad.fractional.auto.forward.DeriverFractionalForward
import scalagrad.fractional.auto.reverse.DeriverFractionalReverse
import scalagrad.fractional.auto.forward.dual.DualNumber
import scalagrad.fractional.auto.reverse.dual.DualDelta
import scalagrad.showcase.deeplearning.Util.*

@main def neuralNetworkSpire() = 
    ???