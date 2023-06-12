package scalagrad.auto.forward.dual

import scalagrad.api.Dual
import scalagrad.api.CreateDual

import scala.math.Fractional.Implicits.given

case class DualNumber[T](value: T, derivative: T)(using f: Fractional[T]) extends Dual[T, T, DualNumber[T]]:

  inline override def v = value
  inline override def dv = derivative

  val cd: CreateDual[T, T, DualNumber[T]] = summon[CreateDual[T, T, DualNumber[T]]]

  override def addD(d1: T, d2: T): T = d1 + d2
  override def subD(d1: T, d2: T): T = d1 - d2
  override def scale(d: T, p: T): T = d * p
  override def inverseScale(d: T, p: T): T = d / p

object DualNumber:

  given[T](using f: Fractional[T]): CreateDual[T, T, DualNumber[T]] = new CreateDual[T, T, DualNumber[T]] {
    override def create(p: T, d: T): DualNumber[T] = DualNumber(p, d)
    override def createEmpty(p: T): DualNumber[T] = DualNumber(p, f.zero)
  }

  type DD = [T] =>> Dual[T, T, DualNumber[T]]