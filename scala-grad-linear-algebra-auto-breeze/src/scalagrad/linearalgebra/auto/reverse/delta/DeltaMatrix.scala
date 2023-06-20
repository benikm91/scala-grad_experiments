package scalagrad.linearalgebra.auto.reverse.delta

import breeze.linalg.{Transpose, DenseVector, DenseMatrix}
import scalagrad.linearalgebra.auto.reverse.dual.{DualDeltaScalar, DualDeltaColumnVector, DualDeltaRowVector}
import scalagrad.linearalgebra.auto.forward.dual.{DualNumberScalar, DualNumberRowVector}
import scalagrad.api.linearalgebra.LinearAlgebraOps

enum DeltaMatrix[P](private[scalagrad] var index: Int = -1):
    case Zero(x: P) extends DeltaMatrix[P]
    case Val(id: Int) extends DeltaMatrix[P]
    case Let(id: Int, rhs: Deltas[P], body: DeltaMatrix[P]) extends DeltaMatrix[P]
    case Transpose(m: DeltaMatrix[P]) extends DeltaMatrix[P]
    case MatrixDotMDM(m: DenseMatrix[P], d: DeltaMatrix[P]) extends DeltaMatrix[P]
    case MatrixDotDMM(d: DeltaMatrix[P], m: DenseMatrix[P]) extends DeltaMatrix[P]
    case MatrixDotMDS(m: DenseMatrix[P], s: DeltaScalar[P]) extends DeltaMatrix[P]
    case MatrixDotDMS(d: DeltaMatrix[P], s: P) extends DeltaMatrix[P]
    case MatrixDotDCVRV(d: DeltaColumnVector[P], v: breeze.linalg.Transpose[DenseVector[P]]) extends DeltaMatrix[P]
    case MatrixDotCVDRV(v: DenseVector[P], d: DeltaRowVector[P]) extends DeltaMatrix[P]
    case AddDMDM(m1: DeltaMatrix[P], m2: DeltaMatrix[P]) extends DeltaMatrix[P]
    case AddDMDS(m: DeltaMatrix[P], s: DeltaScalar[P]) extends DeltaMatrix[P]
    case AddDMDRV(m: DeltaMatrix[P], v: DeltaRowVector[P]) extends DeltaMatrix[P]
    case AddDMDCV(m: DeltaMatrix[P], v: DeltaColumnVector[P]) extends DeltaMatrix[P]
    case MinusDMDM(dm1: DeltaMatrix[P], dm2: DeltaMatrix[P]) extends DeltaMatrix[P]
    case MinusDMDCV(dm: DeltaMatrix[P], dcv: DeltaColumnVector[P]) extends DeltaMatrix[P]
    case MinusDMDRV(dm: DeltaMatrix[P], drv: DeltaRowVector[P]) extends DeltaMatrix[P]
    case MinusDMDS(dm: DeltaMatrix[P], ds: DeltaScalar[P]) extends DeltaMatrix[P]
    case Div(m: DeltaMatrix[P], s: P) extends DeltaMatrix[P]
    case ElementWiseScale(v: DenseMatrix[P], d: DeltaMatrix[P]) extends DeltaMatrix[P]
    case FromElements(values: Vector[DeltaScalar[P]]) extends DeltaMatrix[P]
    case StackDRows(rows: Seq[DeltaRowVector[P]]) extends DeltaMatrix[P]
    case ElementWiseOps(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualDeltaScalar[P] => DualDeltaScalar[P]) extends DeltaMatrix[P]
    case ColumnWiseOps(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualDeltaColumnVector[P] => DualDeltaColumnVector[P]) extends DeltaMatrix[P]
    case RowWiseOps(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualDeltaRowVector[P] => DualDeltaRowVector[P]) extends DeltaMatrix[P]
    case ElementWiseOpsForward(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualNumberScalar[P] => DualNumberScalar[P]) extends DeltaMatrix[P]
    case RowWiseOpsForward(v: DenseMatrix[P], d: DeltaMatrix[P], op: DualNumberRowVector[P] => DualNumberRowVector[P]) extends DeltaMatrix[P]
    case RowWiseOpsManual(v: DenseMatrix[P], d: DeltaMatrix[P], dOp: breeze.linalg.Transpose[DenseVector[P]] => DenseMatrix[P]) extends DeltaMatrix[P]


object DeltaMatrix:

    def zero[P](using frac: Fractional[P]) = DeltaMatrix.Zero(frac.zero)