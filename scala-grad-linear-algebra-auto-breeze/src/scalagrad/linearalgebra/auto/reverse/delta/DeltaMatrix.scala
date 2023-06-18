package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector, DenseMatrix}
import scalagrad.linearalgebra.auto.reverse.dual.{DualDeltaScalar, DualDeltaColumnVector}
import scalagrad.linearalgebra.auto.forward.dual.DualNumberScalar
import scalagrad.linearalgebra.auto.reverse.dual.DualDeltaRowVector

enum DeltaMatrix[P]:
    case Zero(x: P)
    case Val(id: Int)
    case Let(id: Int, rhs: Deltas[P], body: DeltaMatrix[P])
    case Transpose(m: DeltaMatrix[P])
    case MatrixDotMDM(m: DenseMatrix[P], d: DeltaMatrix[P])
    case MatrixDotDMM(d: DeltaMatrix[P], m: DenseMatrix[P])
    case MatrixDotMDS(m: DenseMatrix[P], s: DeltaScalar[P])
    case MatrixDotDMS(d: DeltaMatrix[P], s: P)
    case MatrixDotDCVRV(d: DeltaColumnVector[P], v: breeze.linalg.Transpose[DenseVector[P]])
    case MatrixDotCVDRV(v: DenseVector[P], d: DeltaRowVector[P])
    case AddDMDM(m1: DeltaMatrix[P], m2: DeltaMatrix[P])
    case AddDMDS(m: DeltaMatrix[P], s: DeltaScalar[P])
    case AddDMDRV(m: DeltaMatrix[P], v: DeltaRowVector[P])
    case AddDMDCV(m: DeltaMatrix[P], v: DeltaColumnVector[P])
    case MinusDMDM(dm1: DeltaMatrix[P], dm2: DeltaMatrix[P])
    case MinusDMDCV(dm: DeltaMatrix[P], dcv: DeltaColumnVector[P])
    case MinusDMDRV(dm: DeltaMatrix[P], drv: DeltaRowVector[P])
    case MinusDMDS(dm: DeltaMatrix[P], ds: DeltaScalar[P])
    case Div(m: DeltaMatrix[P], s: P)
    case ElementWiseScale(v: DenseMatrix[P], d: DeltaMatrix[P])
    case FromElements(values: Vector[DeltaScalar[P]])
    case StackDRows(rows: Seq[DeltaRowVector[P]])
    case ElementWiseOps(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualDeltaScalar[P] => DualDeltaScalar[P])
    case ColumnWiseOps(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualDeltaColumnVector[P] => DualDeltaColumnVector[P])
    case RowWiseOps(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualDeltaRowVector[P] => DualDeltaRowVector[P])
    case ElementWiseOpsForward(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualNumberScalar[P] => DualNumberScalar[P])
