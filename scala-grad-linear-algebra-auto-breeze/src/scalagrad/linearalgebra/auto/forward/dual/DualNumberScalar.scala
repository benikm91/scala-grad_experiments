package scalagrad.linearalgebra.auto.forward.dual

import scalagrad.linearalgebra.api.dual.DualScalar

case class DualNumberScalar[T](value: T, derivative: T) extends DualScalar[T, T]:
    def v = value
    def dv = derivative