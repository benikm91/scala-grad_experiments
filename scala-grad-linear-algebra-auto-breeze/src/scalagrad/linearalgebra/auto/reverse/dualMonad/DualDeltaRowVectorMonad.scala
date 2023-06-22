package scalagrad.linearalgebra.auto.reverse.dualMonad

import scala.math.Fractional
import scalagrad.linearalgebra.api.dual.DualRowVector
import scalagrad.linearalgebra.auto.reverse.delta.DeltaRowVector
import breeze.linalg.{DenseVector, Transpose}

case class DualDeltaRowVectorMonad[P: Fractional](
    val value: Transpose[DenseVector[P]], 
    val delta: DeltaMonad[P, DeltaRowVector[P]]
) extends DualRowVector[P, DeltaMonad[P, DeltaRowVector[P]]]:
    def v = value
    def dv = delta