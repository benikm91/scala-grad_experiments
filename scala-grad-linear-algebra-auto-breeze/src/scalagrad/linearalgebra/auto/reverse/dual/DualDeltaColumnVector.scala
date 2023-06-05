package scalagrad.linearalgebra.auto.reverse.dual

import scala.math.Fractional
import scalagrad.linearalgebra.api.dual.DualColumnVector
import scalagrad.linearalgebra.auto.reverse.delta.DeltaColumnVector
import breeze.linalg.DenseVector

case class DualDeltaColumnVector[P: Fractional](val value: DenseVector[P], val delta: DeltaColumnVector[P]) extends DualColumnVector[P, DeltaColumnVector[P]]:
    def v = value
    def dv = delta