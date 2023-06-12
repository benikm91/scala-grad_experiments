package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector, DenseMatrix}

enum DeltaMatrix[P]:
    case Zero(x: P)
    case Val(id: Int)
    case Let(id: Int, rhs: Deltas[P], body: DeltaMatrix[P])
    case Transpose(m: DeltaMatrix[P])
    case MatrixDotMDM(m: DenseMatrix[P], d: DeltaMatrix[P])
    case MatrixDotDMM(d: DeltaMatrix[P], m: DenseMatrix[P])
    case MatrixDotDCVRV(d: DeltaColumnVector[P], v: breeze.linalg.Transpose[DenseVector[P]])
    case MatrixDotCVDRV(v: DenseVector[P], d: DeltaRowVector[P])
    case AddDMDM(m1: DeltaMatrix[P], m2: DeltaMatrix[P])
    case AddDMDCV(m: DeltaMatrix[P], v: DeltaColumnVector[P])
    case ElementWiseScale(v: DenseMatrix[P], d: DeltaMatrix[P])
    case FromElements(values: Vector[DeltaScalar[P]])
