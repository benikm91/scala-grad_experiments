package scalagrad.linearalgebra.auto.reverse.dualMonad

import scalagrad.linearalgebra.api.dual.DualScalar
import scalagrad.linearalgebra.auto.reverse.delta.DeltaScalar

import scalagrad.api.CreateDual
import DeltaState.deltaLet
import DualDeltaScalarMonad.D

case class DualDeltaScalarMonad[P: Fractional](
    val value: P,
    val delta: DeltaMonad[P, DeltaScalar[P]]
) extends DualScalar[P, DeltaMonad[P, DeltaScalar[P]], DualDeltaScalarMonad[P]]:
  inline override def v = value

  inline override def dv = delta

  val cd = summon[CreateDual[P, D[P], DualDeltaScalarMonad[P]]]

  override def addD(d1: D[P], d2: D[P]): D[P] =
    for {
      dx <- d1
      dy <- d2
      newId <- deltaLet(DeltaScalar.Add(dx, dy))
    } yield DeltaScalar.Val(newId)

  override def subD(d1: D[P], d2: D[P]): D[P] =
    for {
      dx <- d1
      dy <- d2
      newId <- deltaLet(DeltaScalar.Sub(dx, dy))
    } yield DeltaScalar.Val(newId)

  override def scale(d: D[P], p: P): D[P] =
    for {
      dv <- d
      newId <- deltaLet(DeltaScalar.Scale(p, dv))
    } yield DeltaScalar.Val(newId)

  override def inverseScale(d: D[P], p: P): D[P] =
    for {
      dv <- d
      newId <- deltaLet(DeltaScalar.Div(dv, p))
    } yield DeltaScalar.Val(newId)

object DualDeltaScalarMonad:

  type D[P] = DeltaMonad[P, DeltaScalar[P]]

  def ZeroM[P](using f: Fractional[P]): D[P] = DeltaMonad[P, DeltaScalar[P]](state => (state, DeltaScalar.Zero(f.zero)))

  given create[P](using f: Fractional[P]): CreateDual[P, D[P], DualDeltaScalarMonad[P]] =
    new CreateDual[P, D[P], DualDeltaScalarMonad[P]] {
      override def create(p: P, d: D[P]): DualDeltaScalarMonad[P] =
        DualDeltaScalarMonad(p, d)
      override def createEmpty(p: P): DualDeltaScalarMonad[P] =
        DualDeltaScalarMonad(p, ZeroM[P])
    }