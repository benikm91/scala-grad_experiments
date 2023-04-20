package scalagrad.auto.reverse.dual

import scalagrad.auto.reverse.dual.delta.{DeltaMonad, Delta, DeltaId, DeltaState}
import scalagrad.api.CreateDual

import scala.math.Fractional.Implicits.given
import scalagrad.api.Dual

import DualDelta.D

case class DualDelta[P: Fractional](value: P, deltaM: D[P]) extends Dual[P, D[P], DualDelta[P]](value, deltaM):
  
  val cd = summon[CreateDual[P, D[P], DualDelta[P]]]

  def deltaLet[P](delta: Delta[P]): DeltaMonad[P, DeltaId] = DeltaMonad[P, DeltaId](next => 
    next._3.get(delta) match
      case None => (DeltaState(next._1 + 1, (next._1, delta) :: next.bindings, next._3 + (delta -> next._1)), next._1)
      case Some(value) => (next, value)
  )

  override def addD(d1: D[P], d2: D[P]): D[P] = 
      for {
        dx <- d1
        dy <- d2
        newId <- deltaLet(dx + dy)
      } yield Delta.Val(newId)
  
  override def subD(d1: D[P], d2: D[P]): D[P] = 
      for {
        dx <- d1
        dy <- d2
        newId <- deltaLet(dx - dy)
      } yield Delta.Val(newId)
  
  override def scale(d: D[P], p: P): D[P] = 
    for {
      dv <- d
      newId <- deltaLet(dv * Delta.Const(p))
    } yield Delta.Val(newId)

  override def inverseScale(d: D[P], p: P): D[P] = 
    for {
      dv <- d
      newId <- deltaLet(dv / Delta.Const(p))
    } yield Delta.Val(newId)


object DualDelta:

  type D[P] = DeltaMonad[P, Delta[P]]

  def ZeroM[P]: D[P] = DeltaMonad[P, Delta[P]](state => (state, Delta.Zero))

  given [P](using f: Fractional[P]): CreateDual[P, D[P], DualDelta[P]] = new CreateDual[P, D[P], DualDelta[P]] {
    override def create(p: P, d: D[P]): DualDelta[P] = DualDelta(p, d)
    override def createEmpty(p: P): DualDelta[P] = DualDelta(p, ZeroM[P])
  }
