package scalagrad.linearalgebra.auto.reverse.dual

import scalagrad.linearalgebra.api.dual.DualScalar
import scalagrad.linearalgebra.auto.reverse.delta.DeltaScalar

case class DualDeltaScalar[P: Fractional](
    val value: P, 
    val delta: DeltaMonad[P, DeltaScalar[P]]
) extends DualScalar[P, DeltaMonad[P, DeltaScalar[P]]]:
    def v = value
    def dv = delta