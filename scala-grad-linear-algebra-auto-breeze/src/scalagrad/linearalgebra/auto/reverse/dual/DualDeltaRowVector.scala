package scalagrad.linearalgebra.auto.reverse.dual

import scala.math.Fractional
import scalagrad.linearalgebra.api.dual.DualRowVector
import scalagrad.linearalgebra.auto.reverse.delta.DeltaRowVector
import breeze.linalg.{DenseVector, Transpose}

case class DualDeltaRowVector[P: Fractional](
    val value: Transpose[DenseVector[P]], 
    val delta: DeltaRowVector[P],
) extends DualRowVector[P, DeltaRowVector[P]]:
    def v = value
    def dv = delta