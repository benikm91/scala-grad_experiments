package scalagrad.linearalgebra.auto.reverse

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
import scalagrad.api.linearalgebra.LinearAlgebraOps
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.fractional.auto.dual.DualIsFractional.given
import scalagrad.auto.reverse.dual.DualDelta
import breeze.linalg.{Vector => _, *}
import breeze.linalg.operators._
import breeze.linalg.support._
import scalagrad.api.Deriver
import scalagrad.api.DeriverPlan
import scala.reflect.ClassTag
import scala.math.Fractional
import scala.runtime.Tuples
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.linearalgebra.api.BreezeVectorAlgebraForDualDouble
import scalagrad.linearalgebra.auto.reverse.dual.*
import scalagrad.linearalgebra.auto.reverse.delta.*
import scalagrad.linearalgebra.auto.forward.dual.{DualNumberScalar, DualNumberRowVector}
import scalagrad.linearalgebra.api.dual.DualScalar
import scalagrad.api.CreateDual

object BreezeVectorAlgebraForDualDeltaDouble extends BreezeVectorAlgebraForDualDouble:

    override type ScalarD = DeltaScalar[Double]
    override type ColumnVectorD = DeltaColumnVector[Double]

    override type RowVectorD = DeltaRowVector[Double]

    override type MatrixD = DeltaMatrix[Double]

    override type Scalar = DualDeltaScalar[Double]
    override type ColumnVector = DualDeltaColumnVector[Double]
    override type RowVector = DualDeltaRowVector[Double]
    override type Matrix = DualDeltaMatrix[Double]

    override given cd: CreateDual[Double, ScalarD, Scalar] = DualDeltaScalar.create

    def liftMatrix(m: DenseMatrix[Double]): Matrix = DualDeltaMatrix(m, DeltaMatrix.zero)

    def liftColumnVector(m: DenseVector[Double]): ColumnVector = DualDeltaColumnVector(m, DeltaColumnVector.zero)

    override def createColumnVector(value: DenseVector[Double], dual: ColumnVectorD): ColumnVector = DualDeltaColumnVector(value, dual)
 
    override def createRowVector(value: Transpose[DenseVector[Double]], dual: RowVectorD): RowVector = DualDeltaRowVector(value, dual)

    override def createMatrix(value: DenseMatrix[Double], dual: MatrixD): Matrix = DualDeltaMatrix(value, dual)
    
    override def zeroD: ScalarD = DeltaScalar.Zero(0.0)
    
    override def dTranspose(dm: MatrixD): MatrixD = DeltaMatrix.Transpose(dm)

    override def dTransposeColumVector(dv: ColumnVectorD): RowVectorD = DeltaRowVector.Transpose(dv)

    override def dTransposeRowVector(dv: RowVectorD): ColumnVectorD = DeltaColumnVector.Transpose(dv)

    override def timesDMM(dm: MatrixD, m: DenseMatrix[Double]): MatrixD = DeltaMatrix.MatrixDotDMM(dm, m)

    override def timesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD = DeltaMatrix.MatrixDotMDM(m, dm)

    override def addDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = DeltaMatrix.AddDMDM(dm1, dm2)

    override def timesDMCV(dm: MatrixD, v: DenseVector[Double]): ColumnVectorD = DeltaColumnVector.MatrixDotDMCV(dm, v)

    override def timesMDCV(m: DenseMatrix[Double], dv: ColumnVectorD): ColumnVectorD = DeltaColumnVector.MatrixDotMDCV(m, dv)
        
    override def addDCVDCV(dv1: ColumnVectorD, dv2: ColumnVectorD): ColumnVectorD = DeltaColumnVector.AddDCVDCV(dv1, dv2)


    override def timesDMS(dm: MatrixD, s: Double): MatrixD = DeltaMatrix.MatrixDotDMS(dm, s)

    override def timesMDS(m: DenseMatrix[Double], ds: ScalarD): MatrixD = DeltaMatrix.MatrixDotMDS(m, ds)
    
    override def timesDCVRV(dv: ColumnVectorD, v: Transpose[DenseVector[Double]]): MatrixD = DeltaMatrix.MatrixDotDCVRV(dv, v)

    override def timesCVDRV(v: DenseVector[Double], dv: RowVectorD): MatrixD = DeltaMatrix.MatrixDotCVDRV(v, dv)

    override def timesDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = DeltaColumnVector.MatrixDotDCVS(dv, s)

    override def timesCVDS(v: DenseVector[Double], ds: ScalarD): ColumnVectorD = DeltaColumnVector.MatrixDotCVDS(v, ds)

    override def addDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD = DeltaRowVector.AddVV(dv1, dv2)

    override def timesDRVM(drv: RowVectorD, m: DenseMatrix[Double]): RowVectorD = DeltaRowVector.MatrixDotDRVM(drv, m)

    override def timesRVDM(rv: Transpose[DenseVector[Double]], dm: MatrixD): RowVectorD = DeltaRowVector.MatrixDotRVDM(rv, dm)

    override def addDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = DeltaScalar.Add(ds1, ds2)
    
    override def timesDRVCV(dv: RowVectorD, v: DenseVector[Double]): ScalarD = DeltaScalar.MatrixDotDRVCV(dv, v)
        
    override def timesRVDCV(v: Transpose[DenseVector[Double]], dv: ColumnVectorD): ScalarD = DeltaScalar.MultiplyRVDCV(v, dv)

    override def timesDRVS(dv: RowVectorD, s: Double): RowVectorD = DeltaRowVector.Scale(dv, s)

    override def timesRVDS(v: Transpose[DenseVector[Double]], ds: ScalarD): RowVectorD = DeltaRowVector.MatrixDotRVDS(v, ds)
        
    override def addDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD = DeltaMatrix.AddDMDCV(dm, dv)

    override def addDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD = DeltaMatrix.AddDMDRV(dm, dv)

    override def addDMDS(dm: MatrixD, ds: ScalarD): MatrixD = DeltaMatrix.AddDMDS(dm, ds)

    override def addDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = DeltaColumnVector.AddVS(dv, ds)

    override def addDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD = DeltaRowVector.AddDRVDS(dv, ds)

    override def subDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = DeltaMatrix.MinusDMDM(dm1, dm2)
        
    override def subDMDCV(dm: MatrixD, dcv: ColumnVectorD): MatrixD = DeltaMatrix.MinusDMDCV(dm, dcv)

    override def subDMDRV(dm: MatrixD, drv: RowVectorD): MatrixD = DeltaMatrix.MinusDMDRV(dm, drv)

    override def subDMDS(dm: MatrixD, ds: ScalarD): MatrixD = DeltaMatrix.MinusDMDS(dm, ds)

    override def subDCVDCV(dv1: ColumnVectorD, dv2: ColumnVectorD): ColumnVectorD = DeltaColumnVector.MinusVV(dv1, dv2)

    override def subDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = DeltaColumnVector.MinusVS(dv, ds)
        
    override def subDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD = DeltaRowVector.MinusDRVDRV(dv1, dv2)

    override def subDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD = DeltaRowVector.MinusDRVDS(dv, ds)

    override def subDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = DeltaScalar.Sub(ds1, ds2)

    override def divideDMS(dm: MatrixD, s: Double): MatrixD = DeltaMatrix.Div(dm, s)

    override def divideDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = DeltaColumnVector.Div(dv, s)

    override def divideDRVS(dv: RowVectorD, s: Double): RowVectorD = DeltaRowVector.Div(dv, s)
    
    override def sumDCV(dv: ColumnVectorD, vLength: Int): ScalarD = DeltaScalar.Sum(dv, vLength)

    override def sumDM(dm: MatrixD, nRows: Int, nCols: Int): ScalarD = DeltaScalar.SumM(dm, nRows, nCols)

    override def foldLeftCV(s: Scalar)(v: ColumnVector)(f: (Scalar, Scalar) => Scalar): Scalar =
        var current = s
        for(i <- 0 until v.length)
            f(current, v.elementAt(i))
        current

    override def elementWiseTimesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD = DeltaMatrix.ElementWiseScale(m, dm)

    override def elementWiseTimesCVDCV(v: DenseVector[Double], dv: ColumnVectorD): ColumnVectorD = DeltaColumnVector.ElementWiseScale(v, dv)

    override def elementAtDM(dm: MatrixD, iRow: Int, jCol: Int, nRows: Int, nCols: Int): ScalarD = DeltaScalar.ElementAtM(dm, iRow, jCol, nRows, nCols)

    override def rowAtDM(dm: MatrixD, rowI: Int, nRows: Int, nCols: Int): RowVectorD = DeltaRowVector.RowAtM(dm, rowI, nRows, nCols)

    override def elementAtDCV(dv: ColumnVectorD, index: Int, length: Int): ScalarD = DeltaScalar.ElementAtCV(dv, index, length)

    override def elementAtDRV(dv: DeltaRowVector[Double], index: Int, length: Int): ScalarD = DeltaScalar.ElementAtRV(dv, index, length)

    override def elementWiseOpsM(m: Matrix, f: Scalar => Scalar): Matrix =
        createMatrix(
            new DenseMatrix(m.rows, m.cols, m.v.map(x => f(DualDeltaScalar(x, DeltaScalar.zero)).v).toArray),
            DeltaMatrix.ElementWiseOps(m.v, m.dv, f)
        )

    override def columnWiseOpsM(m: Matrix, f: ColumnVector => ColumnVector): Matrix =
        createMatrix(
            new DenseMatrix(m.rows, m.cols, m.v.toArray),
            DeltaMatrix.ColumnWiseOps(m.v, m.dv, f)
        )

    override def rowWiseOpsM(m: Matrix, op: RowVector => RowVector): Matrix =
        val mv2 = m.v.copy
        for (r <- 0 until mv2.rows) {
            val c = op(createRowVector(mv2(r, ::), DeltaRowVector.zero))
            mv2(r, ::) := c.v
        }
        createMatrix(
            mv2, 
            DeltaMatrix.RowWiseOps(m.v, m.dv, op)
        )

    def rowWiseOpsMForward(m: Matrix, op: DualNumberRowVector[Double] => DualNumberRowVector[Double]): Matrix =
        createMatrix(
            m.v(breeze.linalg.*, ::).map(x => op(DualNumberRowVector(x.t, DenseVector.zeros[Double](x.length).t)).v.t), 
            DeltaMatrix.RowWiseOpsForward(m.v, m.dv, op)
        )
    
    def rowWiseOpsMManual(
        m: Matrix, 
        op: Transpose[DenseVector[Double]] => Transpose[DenseVector[Double]],
        dOp: Transpose[DenseVector[Double]] => DenseMatrix[Double],
    ): Matrix =
        createMatrix(
            m.v(breeze.linalg.*, ::).map(x => op(x.t).t), 
            DeltaMatrix.RowWiseOpsManual(m.v, m.dv, dOp)
        )
        
    def elementWiseOpsMForward(m: Matrix, f: DualNumberScalar[Double] => DualNumberScalar[Double]): Matrix =
        createMatrix(
            new DenseMatrix(m.rows, m.cols, m.v.map(x => f(DualNumberScalar(x, 0.0)).v).toArray),
            DeltaMatrix.ElementWiseOpsForward(m.v, m.dv, f)
        )

    override def elementWiseOpsCV(v: ColumnVector, f: Scalar => Scalar): ColumnVector =
        createColumnVector(
            DenseVector(v.v.toArray.map(x => f(DualDeltaScalar(x, DeltaScalar.zero)).v)),
            DeltaColumnVector.ElementWiseOps(v.v, v.dv, f)
        )
       
    override def elementWiseOpsRV(v: RowVector, f: Scalar => Scalar): RowVector =
        createRowVector(
            Transpose(DenseVector(v.v.inner.toArray.map(x => f(DualDeltaScalar(x, DeltaScalar.zero)).v))),
            DeltaRowVector.ElementWiseOps(v.v, v.dv, f)
        )

    override def fromDElements(nRows: Int, nCols: Int, elements: ScalarD*): MatrixD =
        DeltaMatrix.FromElements(elements.toVector)

    override def stackDRows(rows: RowVectorD*): MatrixD = DeltaMatrix.StackDRows(rows)
