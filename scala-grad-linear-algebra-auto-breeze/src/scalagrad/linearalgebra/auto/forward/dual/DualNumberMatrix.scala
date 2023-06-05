package scalagrad.linearalgebra.auto.forward.dual

import breeze.linalg.DenseMatrix
import scalagrad.linearalgebra.api.dual.DualMatrix

case class DualNumberMatrix[T](value: DenseMatrix[T], derivative: DenseMatrix[T]) extends DualMatrix[T, DenseMatrix[T]]:
    def v = value
    def dv = derivative