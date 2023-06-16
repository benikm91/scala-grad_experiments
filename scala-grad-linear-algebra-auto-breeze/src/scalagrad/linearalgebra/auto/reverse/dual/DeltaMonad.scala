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
    cacheScalar: Map[DeltaScalar[P], DeltaId],
    cacheColumnVector: Map[DeltaColumnVector[P], DeltaId],
    cacheRowVector: Map[DeltaRowVector[P], DeltaId],
    cacheMatrix: Map[DeltaMatrix[P], DeltaId]
):

    def getScalar(delta: DeltaScalar[P]): Option[DeltaId] = cacheScalar.get(delta)
    def getColumnVector(delta: DeltaColumnVector[P]): Option[DeltaId] = cacheColumnVector.get(delta)
    def getRowVector(delta: DeltaRowVector[P]): Option[DeltaId] = cacheRowVector.get(delta)
    def getMatrix(delta: DeltaMatrix[P]): Option[DeltaId] = cacheMatrix.get(delta)

    def addScalar(delta: DeltaScalar[P]): (DeltaState[P], DeltaId) = (
            copy(
                nextFree = nextFree + 1,
                bindings = (nextFree, delta) :: bindings,
                cacheScalar = cacheScalar + (delta -> nextFree)
            ),
            nextFree
        )

    def addColumnVector(delta: DeltaColumnVector[P]): (DeltaState[P], DeltaId) = (
            copy(
                nextFree = nextFree + 1,
                bindings = (nextFree, delta) :: bindings,
                cacheColumnVector = cacheColumnVector + (delta -> nextFree)
            ),
            nextFree
        )

    def addRowVector(delta: DeltaRowVector[P]): (DeltaState[P], DeltaId) = (
            copy(
                nextFree = nextFree + 1,
                bindings = (nextFree, delta) :: bindings,
                cacheRowVector = cacheRowVector + (delta -> nextFree)
            ),
            nextFree
        )

    def addMatrix(delta: DeltaMatrix[P]): (DeltaState[P], DeltaId) = (
            copy(
                nextFree = nextFree + 1,
                bindings = (nextFree, delta) :: bindings,
                cacheMatrix = cacheMatrix + (delta -> nextFree)
            ),
            nextFree
        )

object DeltaState:
    def start[P](
        nextFree: DeltaId,
    ) = DeltaState[P](
        nextFree, 
        List(), 
        Map(),
        Map(),
        Map(),
        Map()
    )
