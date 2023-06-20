package scalagrad.linearalgebra.api

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
import scalagrad.linearalgebra.api.dual.*
import scalagrad.api.CreateDual

trait BreezeVectorAlgebraForDualDouble extends LinearAlgebraOps:
    
    type ScalarD
    type ColumnVectorD
    type RowVectorD
    type MatrixD

    override type Scalar <: DualScalar[Double, ScalarD, Scalar]
    override type ColumnVector <: DualColumnVector[Double, ColumnVectorD]
    override type RowVector <: DualRowVector[Double, RowVectorD]
    override type Matrix <: DualMatrix[Double, MatrixD]

    given cd: CreateDual[Double, ScalarD, Scalar]
    private val numScalar = summon[Numeric[Scalar]]

    def createScalar(value: Double, dual: ScalarD): Scalar = cd.create(value, dual)
    def createColumnVector(value: DenseVector[Double], dual: ColumnVectorD): ColumnVector
    def createRowVector(value: Transpose[DenseVector[Double]], dual: RowVectorD): RowVector
    def createMatrix(value: DenseMatrix[Double], dual: MatrixD): Matrix

    override def nRows(m: Matrix): Int = m.v.rows
    override def nCols(m: Matrix): Int = m.v.cols
    override def lengthColumnVector(v: ColumnVector): Int = v.v.length
    override def lengthRowVector(v: RowVector): Int = v.v.inner.length

    def zeroD: ScalarD

    override def liftToScalar(d: Int): Scalar = createScalar(d.toDouble, zeroD)
    override def liftToScalar(d: Double): Scalar = createScalar(d, zeroD)

    override def scalarToDouble(s: Scalar) = s.v

    override def inverse(m: Matrix): Matrix = ???
    override def determinant(m: Matrix): Scalar = ???

    def dTranspose(m: MatrixD): MatrixD

    override def transpose(m: Matrix): Matrix = 
        createMatrix(m.v.t, dTranspose(m.dv))

    def dTransposeColumVector(v: ColumnVectorD): RowVectorD

    override def transposeColumVector(v: ColumnVector): RowVector =
        createRowVector(v.v.t, dTransposeColumVector(v.dv))

    def dTransposeRowVector(v: RowVectorD): ColumnVectorD

    override def transposeRowVector(v: RowVector): ColumnVector =
        createColumnVector(v.v.t, dTransposeRowVector(v.dv))
    
    def timesDMM(dm: MatrixD, m: DenseMatrix[Double]): MatrixD
    def timesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD
    def addDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD

    override def timesMM(m1: Matrix, m2: Matrix): Matrix =
        def dTimes(v: DenseMatrix[Double], dv: MatrixD, v2: DenseMatrix[Double], dv2: MatrixD): MatrixD =
            addDMDM(timesDMM(dv, v2), timesMDM(v, dv2))
        createMatrix(m1.v * m2.v, dTimes(m1.v, m1.dv, m2.v, m2.dv))
    
    def timesDMCV(dm: MatrixD, v: DenseVector[Double]): ColumnVectorD
    def timesMDCV(m: DenseMatrix[Double], dv: ColumnVectorD): ColumnVectorD
    def addDCVDCV(dm1: ColumnVectorD, dm2: ColumnVectorD): ColumnVectorD

    override def timesMCV(m: Matrix, v: ColumnVector): ColumnVector = 
        def dTimes(v1: DenseMatrix[Double], dv1: MatrixD, v2: DenseVector[Double], dv2: ColumnVectorD): ColumnVectorD =
            addDCVDCV(
                timesDMCV(dv1, v2), 
                timesMDCV(v1, dv2)
            )
        createColumnVector(m.v * v.v, dTimes(m.v, m.dv, v.v, v.dv))

    def timesDMS(dm: MatrixD, s: Double): MatrixD
    def timesMDS(m: DenseMatrix[Double], ds: ScalarD): MatrixD

    override def timesMS(m: Matrix, s: Scalar): Matrix = 
        def dTimes(m: DenseMatrix[Double], dm: MatrixD, s: Double, ds: ScalarD): MatrixD =
            addDMDM(timesMDS(m, ds), timesDMS(dm, s))
        createMatrix(m.v * s.v, dTimes(m.v, m.dv, s.v, s.dv))
     
    def timesDCVRV(dv: ColumnVectorD, v: Transpose[DenseVector[Double]]): MatrixD
    def timesCVDRV(v: DenseVector[Double], dv: RowVectorD): MatrixD

    override def timesCVRV(v1: ColumnVector, v2: RowVector): Matrix =
        def dTimes(v1: DenseVector[Double], dv1: ColumnVectorD, v2: Transpose[DenseVector[Double]], dv2: RowVectorD): MatrixD =
            addDMDM(
                timesDCVRV(dv1, v2), 
                timesCVDRV(v1, dv2)
            )
        createMatrix(v1.v * v2.v, dTimes(v1.v, v1.dv, v2.v, v2.dv))    

    def timesDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD
    def timesCVDS(v: DenseVector[Double], ds: ScalarD): ColumnVectorD

    override def timesCVS(v: ColumnVector, s: Scalar): ColumnVector =
        def dTimes(v: DenseVector[Double], dv: ColumnVectorD, s: Double, ds: ScalarD): ColumnVectorD =
            addDCVDCV(
                timesDCVS(dv, s), 
                timesCVDS(v, ds)
            )
        createColumnVector(v.v * s.v, dTimes(v.v, v.dv, s.v, s.dv))

    def addDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD
    def timesDRVM(dv: RowVectorD, m: DenseMatrix[Double]): RowVectorD
    def timesRVDM(v: Transpose[DenseVector[Double]], dv: MatrixD): RowVectorD

    override def timesRVM(v: RowVector, m: Matrix): RowVector =
        def dTimes(rv: Transpose[DenseVector[Double]], drv: RowVectorD, m: DenseMatrix[Double], dm: MatrixD): RowVectorD =
            addDRVDRV(
                timesDRVM(drv, m), 
                timesRVDM(rv, dm)
            )
        val xx = v.v * m.v
        createRowVector(v.v * m.v, dTimes(v.v, v.dv, m.v, m.dv))    

    def addDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD // TODO remove this function (already implemented in DualScalar)
    def timesDRVCV(dv: RowVectorD, v: DenseVector[Double]): ScalarD
    def timesRVDCV(v: Transpose[DenseVector[Double]], dv: ColumnVectorD): ScalarD

    override def timesRVCV(v1: RowVector, v2: ColumnVector): Scalar = 
        def dTimes(v1: Transpose[DenseVector[Double]], dv1: RowVectorD, v2: DenseVector[Double], dv2: ColumnVectorD): ScalarD =
            addDSDS(
                timesDRVCV(dv1, v2), 
                timesRVDCV(v1, dv2)
            )
        createScalar(v1.v * v2.v, dTimes(v1.v, v1.dv, v2.v, v2.dv))

    def timesDRVS(dv: RowVectorD, s: Double): RowVectorD
    def timesRVDS(v: Transpose[DenseVector[Double]], ds: ScalarD): RowVectorD

    override def timesRVS(v: RowVector, s: Scalar): RowVector =
        def dTimes(v: Transpose[DenseVector[Double]], dv: RowVectorD, s: Double, ds: ScalarD): RowVectorD =
            addDRVDRV(
                timesDRVS(dv, s), 
                timesRVDS(v, ds)
            )
        createRowVector(v.v * s.v, dTimes(v.v, v.dv, s.v, s.dv))

    override def timesSS(s1: Scalar, s2: Scalar): Scalar = numScalar.times(s1, s2)

    override def plusMM(m1: Matrix, m2: Matrix): Matrix = 
        createMatrix(m1.v + m2.v, addDMDM(m1.dv, m2.dv))

    def addDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD

    override def plusMCV(m: Matrix, v: ColumnVector): Matrix =
        createMatrix(m.v(::, breeze.linalg.*) + v.v, addDMDCV(m.dv, v.dv))
        
    def addDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD

    override def plusMRV(m: Matrix, v: RowVector): Matrix =
        createMatrix(m.v(breeze.linalg.*, ::) + v.v.t, addDMDRV(m.dv, v.dv))

    def addDMDS(dm: MatrixD, ds: ScalarD): MatrixD
    def addDSDM(ds: ScalarD, dm: MatrixD): MatrixD = addDMDS(dm, ds)  // commutative

    override def plusMS(m: Matrix, s: Scalar): Matrix = 
        createMatrix(m.v + s.v, addDMDS(m.dv, s.dv))

    override def plusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = 
        createColumnVector(v1.v + v2.v, addDCVDCV(v1.dv, v2.dv))

    def addDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD
    def addDSDCV(ds: ScalarD, dv: ColumnVectorD): ColumnVectorD = addDCVDS(dv, ds)  // commutative

    override def plusCVS(v: ColumnVector, s: Scalar): ColumnVector =
        createColumnVector(v.v + s.v, addDCVDS(v.dv, s.dv))

    override def plusRVRV(v1: RowVector, v2: RowVector): RowVector =
        createRowVector(v1.v + v2.v, addDRVDRV(v1.dv, v2.dv))

    def addDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD
    def addDSDRV(ds: ScalarD, dv: RowVectorD): RowVectorD = addDRVDS(dv, ds)  // commutative

    override def plusRVS(v: RowVector, s: Scalar): RowVector =
        createRowVector(v.v + s.v, addDRVDS(v.dv, s.dv))
      
    override def plusSS(s1: Scalar, s2: Scalar): Scalar = numScalar.plus(s1, s2)

    def subDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD

    override def minusMM(m1: Matrix, m2: Matrix): Matrix =
        createMatrix(m1.v - m2.v, subDMDM(m1.dv, m2.dv))

    def subDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD

    override def minusMCV(m: Matrix, v: ColumnVector): Matrix = 
        createMatrix(m.v(::, breeze.linalg.*) - v.v, subDMDCV(m.dv, v.dv))

    def subDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD

    override def minusMRV(m: Matrix, v: RowVector): Matrix =
        createMatrix(m.v(breeze.linalg.*, ::) - v.v.t, subDMDRV(m.dv, v.dv))    

    def subDMDS(dm: MatrixD, ds: ScalarD): MatrixD

    override def minusMS(m: Matrix, s: Scalar): Matrix =
        createMatrix(m.v - s.v, subDMDS(m.dv, s.dv))

    def subDCVDCV(dv1: ColumnVectorD, dv2: ColumnVectorD): ColumnVectorD

    override def minusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector =
        createColumnVector(v1.v - v2.v, subDCVDCV(v1.dv, v2.dv))

    def subDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD

    override def minusCVS(v: ColumnVector, s: Scalar): ColumnVector =
        createColumnVector(v.v - s.v, subDCVDS(v.dv, s.dv))
    
    def subDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD

    override def minusRVRV(v1: RowVector, v2: RowVector): RowVector = 
        createRowVector(v1.v - v2.v, subDRVDRV(v1.dv, v2.dv))    

    def subDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD

    override def minusRVS(v: RowVector, s: Scalar): RowVector =
        createRowVector(v.v - s.v, subDRVDS(v.dv, s.dv))

    def subDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD

    override def minusSS(s1: Scalar, s2: Scalar): Scalar = numScalar.minus(s1, s2)

    def divideDMS(dm: MatrixD, s: Double): MatrixD

    override def divideMS(m: Matrix, s: Scalar): Matrix =
        def dDivide(m: DenseMatrix[Double], dm: MatrixD, s: Double, ds: ScalarD): MatrixD =
            divideDMS(
                subDMDM(timesDMS(dm, s), timesMDS(m, ds)),
                (s * s)
            )
        createMatrix(m.v / s.v, dDivide(m.v, m.dv, s.v, s.dv))
    
    def divideDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD

    override def divideCVS(v: ColumnVector, s: Scalar): ColumnVector =
        def dDivide(v: DenseVector[Double], dv: ColumnVectorD, s: Double, ds: ScalarD): ColumnVectorD =
            divideDCVS(
                subDCVDCV(timesDCVS(dv, s), timesCVDS(v, ds)),
                (s * s)
            )
        createColumnVector(v.v / s.v, dDivide(v.v, v.dv, s.v, s.dv))
    
    def divideDRVS(dv: RowVectorD, s: Double): RowVectorD

    override def divideRVS(v: RowVector, s: Scalar): RowVector =
        def dDivide(v: Transpose[DenseVector[Double]], dv: RowVectorD, s: Double, ds: ScalarD): RowVectorD =
            divideDRVS(
                subDRVDRV(timesDRVS(dv, s), timesRVDS(v, ds)),
                (s * s)
            )
        createRowVector(v.v / s.v, dDivide(v.v, v.dv, s.v, s.dv))

    override def divideSS(s1: Scalar, s2: Scalar): Scalar = numScalar.div(s1, s2)
    
    def sumDCV(dv: ColumnVectorD, length: Int): ScalarD

    override def sumCV(v: ColumnVector): Scalar =
        createScalar(breeze.linalg.sum(v.v), sumDCV(v.dv, v.v.length))

    def sumDM(dm: MatrixD, nRows: Int, nCols: Int): ScalarD

    override def sumM(m: Matrix): Scalar = 
        createScalar(breeze.linalg.sum(m.v), sumDM(m.dv, m.v.rows, m.v.cols))

    def elementWiseTimesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD
    def elementWiseTimesDMM(dm: MatrixD, m: DenseMatrix[Double]): MatrixD = elementWiseTimesMDM(m, dm)  // commutative

    override def elementWiseTimesMM(m1: Matrix, m2: Matrix): Matrix = 
        def dElementWiseTimesMM(m1: DenseMatrix[Double], dm1: MatrixD, m2: DenseMatrix[Double], dm2: MatrixD): MatrixD =
            addDMDM(
                elementWiseTimesMDM(m1, dm2),
                elementWiseTimesDMM(dm1, m2)
            )
        createMatrix(m1.v *:* m2.v, dElementWiseTimesMM(m1.v, m1.dv, m2.v, m2.dv))

    def elementWiseTimesCVDCV(v: DenseVector[Double], dv: ColumnVectorD): ColumnVectorD
    def elementWiseTimesDCVCV(dv: ColumnVectorD, v: DenseVector[Double]): ColumnVectorD = elementWiseTimesCVDCV(v, dv)  // commutative

    override def elementWiseTimesCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = 
        def dElementWiseTimesCVCV(v1: DenseVector[Double], dv1: ColumnVectorD, v2: DenseVector[Double], dv2: ColumnVectorD): ColumnVectorD =
            addDCVDCV(
                elementWiseTimesDCVCV(dv1, v2),
                elementWiseTimesCVDCV(v1, dv2)
            )
        createColumnVector(v1.v *:* v2.v, dElementWiseTimesCVCV(v1.v, v1.dv, v2.v, v2.dv))
    
    def elementAtDM(dm: MatrixD, iRow: Int, jCol: Int, nRows: Int, nCols: Int): ScalarD

    def elementAtM(m: Matrix, iRow: Int, jCol: Int): Scalar = 
        createScalar(
            m.v(iRow, jCol),
            elementAtDM(m.dv, iRow, jCol, m.rows, m.cols)
        )

    def rowAtDM(dm: MatrixD, rowI: Int, nRows: Int, nCols: Int): RowVectorD

    override def rowAtM(m: Matrix, rowI: Int): RowVector =
        createRowVector(
            m.v(rowI, ::),
            rowAtDM(m.dv, rowI, m.rows, m.cols)
        )

    def elementAtDCV(dv: ColumnVectorD, index: Int, length: Int): ScalarD

    def elementAtCV(v: ColumnVector, index: Int): Scalar = 
        createScalar(
            v.v(index),
            elementAtDCV(v.dv, index, v.length)
        )

    def elementAtDRV(dv: RowVectorD, index: Int, length: Int): ScalarD

    def elementAtRV(v: RowVector, index: Int): Scalar = 
        createScalar(
            v.v(index),
            elementAtDRV(v.dv, index, v.length)
        )

    def stackDRows(rows: RowVectorD*): MatrixD

    override def stackRows(rows: RowVector*): Matrix = 
        createMatrix(
            DenseMatrix.vertcat(rows.map(_.v.t.toDenseMatrix): _*),
            stackDRows(rows.map(_.dv): _*)
        )