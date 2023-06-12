package scalagrad.linearalgebra.auto.forward

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
import scalagrad.linearalgebra.auto.forward.dual.*

object BreezeVectorAlgebraForDualNumberDouble extends BreezeVectorAlgebraForDualDouble:

    override type ScalarD = Double
    override type ColumnVectorD = DenseVector[Double]
    override type RowVectorD = Transpose[DenseVector[Double]]
    override type MatrixD = DenseMatrix[Double]

    override type Scalar = DualNumberScalar[Double]
    override type ColumnVector = DualNumberColumnVector[Double]
    override type RowVector = DualNumberRowVector[Double]
    override type Matrix = DualNumberMatrix[Double]

    override def createScalar(value: Double, dual: ScalarD): Scalar = 
        DualNumberScalar(value, dual)

    override def createColumnVector(value: DenseVector[Double], dual: ColumnVectorD): ColumnVector = 
        DualNumberColumnVector(value, dual)

    override def createRowVector(value: Transpose[DenseVector[Double]], dual: RowVectorD): RowVector = 
        DualNumberRowVector(value, dual)

    override def createMatrix(value: DenseMatrix[Double], dual: MatrixD): Matrix =
        DualNumberMatrix(value, dual)
    
    override def zeroD: ScalarD = 0.0
    
    override def dTranspose(m: MatrixD): MatrixD = m.t
    override def dTransposeColumVector(v: ColumnVectorD): RowVectorD = v.t
    override def dTransposeRowVector(v: RowVectorD): ColumnVectorD = v.t
    override def timesDMM(dm: MatrixD, m: DenseMatrix[Double]): MatrixD = dm * m
    override def timesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD = m * dm
    override def addDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = dm1 + dm2
    override def timesDMCV(dm: MatrixD, v: DenseVector[Double]): ColumnVectorD = dm * v
    override def timesMDCV(m: DenseMatrix[Double], dv: ColumnVectorD): ColumnVectorD = m * dv
    override def addDCVDCV(dm1: ColumnVectorD, dm2: ColumnVectorD): ColumnVectorD = dm1 + dm2
    override def timesDMS(dm: MatrixD, s: Double): MatrixD = dm * s
    override def timesMDS(m: DenseMatrix[Double], ds: ScalarD): MatrixD = m * ds
    override def timesDCVRV(dv: ColumnVectorD, v: Transpose[DenseVector[Double]]): MatrixD = dv * v
    override def timesCVDRV(v: DenseVector[Double], dv: RowVectorD): MatrixD = v * dv
    override def timesDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = dv * s
    override def timesCVDS(v: DenseVector[Double], ds: ScalarD): ColumnVectorD = v * ds
    override def addDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD = dv1 + dv2
    override def timesDRVM(dv: RowVectorD, m: DenseMatrix[Double]): RowVectorD = dv * m
    override def timesRDMV(v: Transpose[DenseVector[Double]], dv: MatrixD): RowVectorD = v * dv
    override def addDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = ds1 + ds2
    override def timesDRVCV(dv: RowVectorD, v: DenseVector[Double]): ScalarD = dv * v
    override def timesRVDCV(v: Transpose[DenseVector[Double]], dv: ColumnVectorD): ScalarD = v * dv
    override def timesDRVS(dv: RowVectorD, s: Double): RowVectorD = dv * s
    override def timesRVDS(v: Transpose[DenseVector[Double]], ds: ScalarD): RowVectorD = v * ds
    override def timesSDS(s: Double, ds: ScalarD): ScalarD = s * ds
    override def addDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD = dm(breeze.linalg.*, ::) + dv
    override def addDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD = dm(::, breeze.linalg.*) + dv.t
    override def addDMDS(dm: MatrixD, ds: ScalarD): MatrixD = dm + ds
    override def addDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = dv + ds
    override def addDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD = dv + ds
    override def subDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = dm1 - dm2
    override def subDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD = dm(breeze.linalg.*, ::) - dv
    override def subDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD = dm(::, breeze.linalg.*) - dv.t
    override def subDMDS(dm: MatrixD, ds: ScalarD): MatrixD = dm - ds
    override def subDCVDCV(dv1: ColumnVectorD, dv2: ColumnVectorD): ColumnVectorD = dv1 - dv2
    override def subDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = dv - ds
    override def subDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD = dv1 - dv2
    override def subDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD = dv - ds
    override def subDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = ds1 - ds2
    override def divideDMS(dm: MatrixD, s: Double): MatrixD = dm / s
    override def divideDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = dv / s
    override def divideDRVS(dv: RowVectorD, s: Double): RowVectorD = dv / s
    override def divideDSS(ds: ScalarD, s: Double): ScalarD = ds / s
    override def sumDCV(dv: ColumnVectorD, vLength: Int): ScalarD = breeze.linalg.sum(dv)

    override def reduceCV(v: ColumnVector)(f: [T] => (T, T) => Numeric[T] ?=> T): Scalar =
        val xx = v.v.toScalaVector.zip(v.dv.toScalaVector)
            .map(DualNumber(_, _))
            .reduce(f[DualNumber[Double]])
        DualNumberScalar(xx.v, xx.dv)

    override def elementAtDM(dm: DenseMatrix[Double], iRow: Int, jCol: Int, nRows: Int, nCols: Int): ScalarD = 
        assert(dm.rows == nRows)
        assert(dm.cols == nCols)
        dm(iRow, jCol)

    override def elementAtDCV(dv: ColumnVectorD, index: Int, length: Int): ScalarD = 
        assert(dv.length == length)
        dv(index)

    override def elementAtDRV(dv: RowVectorD, index: Int, length: Int): ScalarD = 
        assert(dv.t.length == length)
        dv(index)

    override def elementWiseOpsM(m: Matrix, f: Scalar => Scalar): Matrix = 
        val x = m.v.toArray.zip(m.dv.toArray).map((x, dx) => 
            f(createScalar(x, dx))
        )
        createMatrix(
            new DenseMatrix(m.rows, m.cols, x.map(_.v).toArray),
            new DenseMatrix(m.rows, m.cols, x.map(_.dv).toArray)
        )

    override def elementWiseOpsCV(v: ColumnVector, f: Scalar => Scalar): ColumnVector = 
        val x = v.v.toScalaVector.zip(v.dv.toScalaVector).map((x, dx) => 
            f(createScalar(x, dx))
        )
        createColumnVector(
            DenseVector(x.map(_.v).toArray),
            DenseVector(x.map(_.dv).toArray)
        )

    override def elementWiseOpsRV(v: RowVector, f: Scalar => Scalar): RowVector = 
        val x = v.v.t.toScalaVector.zip(v.dv.t.toScalaVector).map((x, dx) => 
            f(createScalar(x, dx))
        )
        createRowVector(
            DenseVector(x.map(_.v).toArray).t,
            DenseVector(x.map(_.dv).toArray).t
        )
