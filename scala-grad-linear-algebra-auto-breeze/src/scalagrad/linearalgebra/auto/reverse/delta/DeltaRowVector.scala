package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector, DenseMatrix}

enum DeltaRowVector[P]:
    case Zero(x: P)
    case Let(id: Int, rhs: Deltas[P], body: DeltaRowVector[P])
    case Val(id: Int)
    case Transpose(v: DeltaColumnVector[P])
    case MatrixDot(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaMatrix[P])
    case MatrixDot2(d: DeltaRowVector[P], v: DenseMatrix[P])
    case AddVV(d1: DeltaRowVector[P], d2: DeltaRowVector[P])
    case FromElements(values: Vector[DeltaScalar[P]])