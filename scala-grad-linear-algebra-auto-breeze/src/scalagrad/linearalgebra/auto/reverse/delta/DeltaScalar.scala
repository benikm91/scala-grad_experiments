package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector}

enum DeltaScalar[P](private[scalagrad] var index: Int = -1):
    case Zero(x: P) extends DeltaScalar[P]
    case Let(id: Int, rhs: Deltas[P], body: DeltaScalar[P]) extends DeltaScalar[P]
    case Val(id: Int) extends DeltaScalar[P]
    case MultiplyVV(m1: DeltaRowVector[P], m2: DeltaColumnVector[P]) extends DeltaScalar[P]
    case MultiplyRVDCV(v: Transpose[DenseVector[P]], d: DeltaColumnVector[P]) extends DeltaScalar[P]
    case MatrixDotDRVCV(d: DeltaRowVector[P], v: DenseVector[P]) extends DeltaScalar[P]
    case ColumnDotProduct(v: Transpose[DenseVector[P]], d: DeltaColumnVector[P]) extends DeltaScalar[P]
    case RowDotProduct(d: DeltaRowVector[P], v: DenseVector[P]) extends DeltaScalar[P]
    case Add(s1: DeltaScalar[P], s2: DeltaScalar[P]) extends DeltaScalar[P]
    case Sub(s1: DeltaScalar[P], s2: DeltaScalar[P]) extends DeltaScalar[P]
    case Div(s1: DeltaScalar[P], f: P) extends DeltaScalar[P]
    case Scale(c: P, ds: DeltaScalar[P]) extends DeltaScalar[P]
    case Sum(v: DeltaColumnVector[P], vectorLength: Int) extends DeltaScalar[P]
    case SumM(m: DeltaMatrix[P], nRows: Int, nCols: Int) extends DeltaScalar[P]
    case ElementAtM(m: DeltaMatrix[P], row: Int, col: Int, nRows: Int, nCols: Int) extends DeltaScalar[P]
    case ElementAtCV(delta: DeltaColumnVector[P], i: Int, deltaLength: Int) extends DeltaScalar[P]
    case ElementAtRV(delta: DeltaRowVector[P], i: Int, deltaLength: Int) extends DeltaScalar[P]


object DeltaScalar:

    def zero[P](using frac: Fractional[P]) = DeltaScalar.Zero(frac.zero)