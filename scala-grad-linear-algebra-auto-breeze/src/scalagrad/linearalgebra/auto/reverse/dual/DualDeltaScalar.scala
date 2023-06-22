package scalagrad.linearalgebra.auto.reverse.dual

import scalagrad.linearalgebra.api.dual.DualScalar
import scalagrad.linearalgebra.auto.reverse.delta.DeltaScalar

import scalagrad.api.CreateDual

case class DualDeltaScalar[P: Fractional](
    val value: P,
    val delta: DeltaScalar[P]
) extends DualScalar[P, DeltaScalar[P], DualDeltaScalar[P]]:
  inline override def v = value

  inline override def dv = delta

  val cd = summon[CreateDual[P, DeltaScalar[P], DualDeltaScalar[P]]]

  override def addD(d1: DeltaScalar[P], d2: DeltaScalar[P]): DeltaScalar[P] = DeltaScalar.Add(d1, d2)

  override def subD(d1: DeltaScalar[P], d2: DeltaScalar[P]): DeltaScalar[P] = DeltaScalar.Sub(d1, d2)

  override def scale(d: DeltaScalar[P], p: P): DeltaScalar[P] = DeltaScalar.Scale(p, d)

  override def inverseScale(d: DeltaScalar[P], p: P): DeltaScalar[P] = DeltaScalar.Div(d, p)

object DualDeltaScalar:

  given create[P](using f: Fractional[P]): CreateDual[P, DeltaScalar[P], DualDeltaScalar[P]] =
    new CreateDual[P, DeltaScalar[P], DualDeltaScalar[P]] {
      override def create(p: P, d: DeltaScalar[P]): DualDeltaScalar[P] =
        DualDeltaScalar(p, d)
      override def createEmpty(p: P): DualDeltaScalar[P] =
        DualDeltaScalar(p, DeltaScalar.Zero[P](f.zero))
    }
