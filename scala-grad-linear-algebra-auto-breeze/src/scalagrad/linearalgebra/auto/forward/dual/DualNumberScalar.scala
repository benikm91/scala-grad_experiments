package scalagrad.linearalgebra.auto.forward.dual

import scalagrad.linearalgebra.api.dual.DualScalar
import scalagrad.api.CreateDual

import scala.math.Fractional.Implicits.given

case class DualNumberScalar[T](value: T, derivative: T)(using f: Fractional[T]) extends DualScalar[T, T, DualNumberScalar[T]]:

  inline override def v = value
  inline override def dv = derivative

  val cd: CreateDual[T, T, DualNumberScalar[T]] = summon[CreateDual[T, T, DualNumberScalar[T]]]

  override def addD(d1: T, d2: T): T = d1 + d2
  override def subD(d1: T, d2: T): T = d1 - d2
  override def scale(d: T, p: T): T = d * p
  override def inverseScale(d: T, p: T): T = d / p

object DualNumberScalar:

  given[T](using f: Fractional[T]): CreateDual[T, T, DualNumberScalar[T]] = new CreateDual[T, T, DualNumberScalar[T]] {
    override def create(p: T, d: T): DualNumberScalar[T] = DualNumberScalar(p, d)
    override def createEmpty(p: T): DualNumberScalar[T] = DualNumberScalar(p, f.zero)
  }
