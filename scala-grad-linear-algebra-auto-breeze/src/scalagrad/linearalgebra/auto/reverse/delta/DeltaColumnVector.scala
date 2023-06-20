package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{DenseVector, DenseMatrix}
import scalagrad.linearalgebra.auto.reverse.dual.DualDeltaScalar

enum DeltaColumnVector[P](private[scalagrad] var index: Int = -1):
    case Zero(x: P) extends DeltaColumnVector[P]
    case Val(id: Int) extends DeltaColumnVector[P]
    case Let(id: Int, rhs: Deltas[P], body: DeltaColumnVector[P]) extends DeltaColumnVector[P]
    case Transpose(v: DeltaRowVector[P]) extends DeltaColumnVector[P]
    case MatrixDotDMCV(d: DeltaMatrix[P], v: DenseVector[P]) extends DeltaColumnVector[P]
    case MatrixDotMDCV(v: DenseMatrix[P], d: DeltaColumnVector[P]) extends DeltaColumnVector[P]
    case MatrixDotCVDS(cv: DenseVector[P], ds: DeltaScalar[P]) extends DeltaColumnVector[P]
    case MatrixDotDCVS(d: DeltaColumnVector[P], f: Double) extends DeltaColumnVector[P]
    case Div(d: DeltaColumnVector[P], f: Double) extends DeltaColumnVector[P]
    case AddDCVDCV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P]) extends DeltaColumnVector[P]
    case AddVS(d: DeltaColumnVector[P], s: DeltaScalar[P]) extends DeltaColumnVector[P]
    case MinusVV(d1: DeltaColumnVector[P], d2: DeltaColumnVector[P]) extends DeltaColumnVector[P]
    case MinusVS(d: DeltaColumnVector[P], s: DeltaScalar[P]) extends DeltaColumnVector[P]
    case ElementWiseScale(v: DenseVector[P], d: DeltaColumnVector[P]) extends DeltaColumnVector[P]
    case FromElements(values: Vector[DeltaScalar[P]]) extends DeltaColumnVector[P]
    case ElementWiseOps(v: DenseVector[P], d: DeltaColumnVector[P], op: DualDeltaScalar[P] => DualDeltaScalar[P]) extends DeltaColumnVector[P]


object DeltaColumnVector:

    def zero[P](using frac: Fractional[P]) = DeltaColumnVector.Zero(frac.zero)