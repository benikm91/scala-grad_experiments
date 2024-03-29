package scalagrad.linearalgebra.auto.reverse.dual

import scala.math.Fractional
import scalagrad.linearalgebra.api.dual.DualMatrix
import scalagrad.linearalgebra.auto.reverse.delta.DeltaMatrix
import breeze.linalg.DenseMatrix

case class DualDeltaMatrix[P: Fractional](
    val value: DenseMatrix[P], 
    val delta: DeltaMatrix[P],
) extends DualMatrix[P, DeltaMatrix[P]]:
    def v = value
    def dv = delta
    