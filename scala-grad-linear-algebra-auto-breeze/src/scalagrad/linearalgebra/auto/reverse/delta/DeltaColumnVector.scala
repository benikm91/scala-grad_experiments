package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{DenseVector, DenseMatrix}

enum DeltaColumnVector[P]:
    case Zero(x: P)
    case Val(id: Int)
    case Let(id: Int, rhs: Deltas[P], body: DeltaColumnVector[P])
    case Transpose(v: DeltaRowVector[P])
    case MatrixDot(d: DeltaMatrix[P], v: DenseVector[P])
    case MatrixDot2(v: DenseMatrix[P], d: DeltaColumnVector[P])
    case AddVV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P])
    case AddVS(d: DeltaColumnVector[P], s: DeltaScalar[P])
    case MinusVV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P])
    case ElementWiseScale(v: DenseVector[P], d: DeltaColumnVector[P])