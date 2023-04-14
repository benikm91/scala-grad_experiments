package scalagrad.reverse.dual.delta

import scalagrad.reverse.dual.delta.Delta
// Must be like: type M a = DeltaState → (a, DeltaState)
/** DeltaMonad is a simple State Monad */
case class DeltaMonad[P, A](private val next: DeltaState[P] => (DeltaState[P], A)):

    def map[B](f: A => B): DeltaMonad[P, B] = 
        DeltaMonad(state => 
            val (nextState, a) = run(state)
            (nextState, f(a))
        )

    def flatMap[B](f: A => DeltaMonad[P, B]): DeltaMonad[P, B] = 
        DeltaMonad(state => 
            val (nextState, a) = run(state)
            f(a).run(nextState)
        )

    def run(state: DeltaState[P]): (DeltaState[P], A) = next(state)


type DeltaId = Int
type DeltaBindings[P] = List[(DeltaId, Delta[P])]

case class DeltaState[P](nextFree: DeltaId, bindings: DeltaBindings[P], cache: Map[Delta[P], DeltaId])

object DeltaState:
    def start[P](nextFree: DeltaId) = DeltaState[P](nextFree, List(), Map())