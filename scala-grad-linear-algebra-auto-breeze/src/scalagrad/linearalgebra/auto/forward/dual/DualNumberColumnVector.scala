package scalagrad.linearalgebra.auto.forward.dual

import breeze.linalg.DenseVector
import scalagrad.linearalgebra.api.dual.DualColumnVector

case class DualNumberColumnVector[T](value: DenseVector[T], derivative: DenseVector[T]) extends DualColumnVector[T, DenseVector[T]]:
    def v = value
    def dv = derivative