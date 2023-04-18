package scalagrad.auto.reverse.dual

import scalagrad.auto.reverse.dual.delta.{DeltaMonad, Delta, DeltaId, DeltaState}


case class DualDelta[P](value: P, deltaM: DeltaMonad[P, Delta[P]]):
  inline def v: P = value

object DualDelta:

  def ZeroM[P] = DeltaMonad[P, Delta[P]](state => (state, Delta.Zero))
