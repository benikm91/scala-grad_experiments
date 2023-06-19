package scalagrad.linearalgebra.auto.reverse.dual

import scalagrad.linearalgebra.auto.reverse.delta.*

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


object DeltaMonad:

    def zeroS: DeltaMonad[Double, DeltaScalar[Double]] = 
        DeltaMonad(state => (state, DeltaScalar.Zero(0d)))

    def zeroCV: DeltaMonad[Double, DeltaColumnVector[Double]] = 
        DeltaMonad(state => (state, DeltaColumnVector.Zero(0d)))

    def zeroRV: DeltaMonad[Double, DeltaRowVector[Double]] =
        DeltaMonad(state => (state, DeltaRowVector.Zero(0d)))

    def zeroM: DeltaMonad[Double, DeltaMatrix[Double]] =
        DeltaMonad(state => (state, DeltaMatrix.Zero(0d)))

    def pure[P, A](a: A): DeltaMonad[P, A] = DeltaMonad(state => (state, a))

    def traverse[A, B](as: List[DeltaMonad[Double, A]]): DeltaMonad[Double, List[A]] =
        as.foldRight(pure[Double, List[A]](List()))((a, acc) => 
            for {
                x <- a
                xs <- acc
            } yield x :: xs
        )
        

type DeltaId = Int
type DeltaBindings[P] = List[(DeltaId, Deltas[P])]

case class DeltaState[P](
    nextFree: DeltaId, 
    bindings: DeltaBindings[P], 
)

object DeltaState:
    def start[P](
        nextFree: DeltaId,
    ) = DeltaState[P](
        nextFree, 
        List(),
    )

    def deltaLet[P](delta: Deltas[P]): DeltaMonad[P, DeltaId] = DeltaMonad[P, DeltaId](next => (
        next.copy(
            nextFree = next.nextFree + 1,
            bindings = (next.nextFree, delta) :: next.bindings,
        ),
        next.nextFree
    ))

