package scalagrad.linearalgebra.auto.reverse.dual

import scalagrad.linearalgebra.api.dual.DualScalar
import scalagrad.linearalgebra.auto.reverse.delta.DeltaScalar

import scalagrad.api.CreateDual
import DualDeltaScalar.D
import DualDeltaScalar.deltaLet

case class DualDeltaScalar[P: Fractional](
    val value: P,
    val delta: DeltaMonad[P, DeltaScalar[P]]
) extends DualScalar[P, DeltaMonad[P, DeltaScalar[P]], DualDeltaScalar[P]]:
  inline override def v = value

  inline override def dv = delta

  val cd = summon[CreateDual[P, D[P], DualDeltaScalar[P]]]

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
      newId <- deltaLet(DeltaScalar.Scale(dv, p))
    } yield DeltaScalar.Val(newId)

  override def inverseScale(d: D[P], p: P): D[P] =
    for {
      dv <- d
      newId <- deltaLet(DeltaScalar.Div(dv, p))
    } yield DeltaScalar.Val(newId)

object DualDeltaScalar:

  type D[P] = DeltaMonad[P, DeltaScalar[P]]

  def ZeroM[P](using f: Fractional[P]): D[P] = DeltaMonad[P, DeltaScalar[P]](state => (state, DeltaScalar.Zero(f.zero)))

  given [P](using f: Fractional[P]): CreateDual[P, D[P], DualDeltaScalar[P]] =
    new CreateDual[P, D[P], DualDeltaScalar[P]] {
      override def create(p: P, d: D[P]): DualDeltaScalar[P] =
        DualDeltaScalar(p, d)
      override def createEmpty(p: P): DualDeltaScalar[P] =
        DualDeltaScalar(p, ZeroM[P])
    }

  def deltaLet[P](delta: DeltaScalar[P]): DeltaMonad[P, DeltaId] =
    DeltaMonad[P, DeltaId](next =>
      next.getScalar(delta) match
        case None        => next.addScalar(delta)
        case Some(value) => (next, value)
    )
