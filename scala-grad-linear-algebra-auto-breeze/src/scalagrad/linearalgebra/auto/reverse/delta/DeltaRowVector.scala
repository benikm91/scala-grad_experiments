package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector, DenseMatrix}
import scalagrad.linearalgebra.auto.reverse.dual.DualDeltaScalar

enum DeltaRowVector[P]:
    case Zero(x: P)
    case Let(id: Int, rhs: Deltas[P], body: DeltaRowVector[P])
    case Val(id: Int)
    case Transpose(v: DeltaColumnVector[P])
    case Scale(drv: DeltaRowVector[P], f: Double)
    case MatrixDotRVDM(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaMatrix[P])
    case MatrixDotDRVM(d: DeltaRowVector[P], m: DenseMatrix[P])
    case MatrixDotRVDS(rv: breeze.linalg.Transpose[DenseVector[P]], ds: DeltaScalar[P])
    case AddVV(d1: DeltaRowVector[P], d2: DeltaRowVector[P])
    case AddDRVDS(drv: DeltaRowVector[P], ds: DeltaScalar[P])
    case MinusDRVDRV(drv1: DeltaRowVector[P], drv2: DeltaRowVector[P])
    case MinusDRVDS(drv: DeltaRowVector[P], ds: DeltaScalar[P])
    case ElementWiseScale(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaRowVector[P])
    case Div(drv: DeltaRowVector[P], f: Double)
    case RowAtM(d: DeltaMatrix[P], row: Int, nRows: Int, nCols: Int)
    case FromElements(values: Vector[DeltaScalar[P]])
    case ElementWiseOps(v: breeze.linalg.Transpose[DenseVector[P]], d: DeltaRowVector[P], op: DualDeltaScalar[P] => DualDeltaScalar[P])