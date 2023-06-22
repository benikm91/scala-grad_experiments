package scalagrad.linearalgebra.auto.reverse.dualMonad

import scala.math.Fractional
import scalagrad.linearalgebra.api.dual.DualColumnVector
import scalagrad.linearalgebra.auto.reverse.delta.DeltaColumnVector
import breeze.linalg.DenseVector

case class DualDeltaColumnVectorMonad[P: Fractional](
    val value: DenseVector[P], 
    val delta: DeltaMonad[P, DeltaColumnVector[P]]
) extends DualColumnVector[P, DeltaMonad[P, DeltaColumnVector[P]]]:
    def v = value
    def dv = delta