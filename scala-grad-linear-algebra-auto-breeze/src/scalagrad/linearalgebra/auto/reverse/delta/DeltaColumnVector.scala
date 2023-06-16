package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{DenseVector, DenseMatrix}
import scalagrad.linearalgebra.auto.reverse.dual.DualDeltaScalar

enum DeltaColumnVector[P]:
    case Zero(x: P)
    case Val(id: Int)
    case Let(id: Int, rhs: Deltas[P], body: DeltaColumnVector[P])
    case Transpose(v: DeltaRowVector[P])
    case MatrixDotDMCV(d: DeltaMatrix[P], v: DenseVector[P])
    case MatrixDotMDCV(v: DenseMatrix[P], d: DeltaColumnVector[P])
    case MatrixDotCVDS(cv: DenseVector[P], ds: DeltaScalar[P])
    case MatrixDotDCVS(d: DeltaColumnVector[P], f: Double)
    case Div(d: DeltaColumnVector[P], f: Double)
    case AddDCVDCV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P])
    case AddVS(d: DeltaColumnVector[P], s: DeltaScalar[P])
    case MinusVV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P])
    case MinusVS(d: DeltaColumnVector[P], s: DeltaScalar[P])
    case ElementWiseScale(v: DenseVector[P], d: DeltaColumnVector[P])
    case FromElements(values: Vector[DeltaScalar[P]])
    case ElementWiseOps(v: DenseVector[P], d: DeltaColumnVector[P], op: DualDeltaScalar[P] => DualDeltaScalar[P])