package scalagrad.linearalgebra.auto.forward.dual

import breeze.linalg.{Transpose, DenseVector}
import scalagrad.linearalgebra.api.dual.DualRowVector

case class DualNumberRowVector[T](value: Transpose[DenseVector[T]], derivative: Transpose[DenseVector[T]]) extends DualRowVector[T, Transpose[DenseVector[T]]]:
    def v = value
    def dv = derivative