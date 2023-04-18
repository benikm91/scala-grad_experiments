package scalagrad.auto.forward.dual

case class DualNumber[T](value: T, derivative: T):
  inline def v: T = value
  inline def dv: T = derivative
