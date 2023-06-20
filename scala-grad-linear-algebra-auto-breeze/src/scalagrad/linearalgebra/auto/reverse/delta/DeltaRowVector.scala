package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector, DenseMatrix}
import scalagrad.linearalgebra.auto.reverse.dual.DualDeltaScalar

enum DeltaRowVector[P](private[scalagrad] var index: Int = -1):
    case Zero(x: P) extends DeltaRowVector[P]
    case Let(id: Int, rhs: Deltas[P], body: DeltaRowVector[P]) extends DeltaRowVector[P]
    case Val(id: Int) extends DeltaRowVector[P]
    case Transpose(v: DeltaColumnVector[P]) extends DeltaRowVector[P]
    case Scale(drv: DeltaRowVector[P], f: Double) extends DeltaRowVector[P]
    case MatrixDotRVDM(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaMatrix[P]) extends DeltaRowVector[P]
    case MatrixDotDRVM(d: DeltaRowVector[P], m: DenseMatrix[P]) extends DeltaRowVector[P]
    case MatrixDotRVDS(rv: breeze.linalg.Transpose[DenseVector[P]], ds: DeltaScalar[P]) extends DeltaRowVector[P]
    case AddVV(d1: DeltaRowVector[P], d2: DeltaRowVector[P]) extends DeltaRowVector[P]
    case AddDRVDS(drv: DeltaRowVector[P], ds: DeltaScalar[P]) extends DeltaRowVector[P]
    case MinusDRVDRV(drv1: DeltaRowVector[P], drv2: DeltaRowVector[P]) extends DeltaRowVector[P]
    case MinusDRVDS(drv: DeltaRowVector[P], ds: DeltaScalar[P]) extends DeltaRowVector[P]
    case ElementWiseScale(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaRowVector[P]) extends DeltaRowVector[P]
    case Div(drv: DeltaRowVector[P], f: Double) extends DeltaRowVector[P]
    case RowAtM(d: DeltaMatrix[P], row: Int, nRows: Int, nCols: Int) extends DeltaRowVector[P]
    case FromElements(values: Vector[DeltaScalar[P]]) extends DeltaRowVector[P]
    case ElementWiseOps(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaRowVector[P], op: DualDeltaScalar[P] => DualDeltaScalar[P]) extends DeltaRowVector[P]


object DeltaRowVector:

    def zero[P](using frac: Fractional[P]) = DeltaRowVector.Zero(frac.zero)