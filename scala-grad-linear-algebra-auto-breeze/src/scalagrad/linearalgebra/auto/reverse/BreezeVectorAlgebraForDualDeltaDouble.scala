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

object BreezeVectorAlgebraForDualDeltaDouble extends BreezeVectorAlgebraForDualDouble:

    override type ScalarD = DeltaScalar[Double]
    override type ColumnVectorD = DeltaColumnVector[Double]
    override type RowVectorD = DeltaRowVector[Double]
    override type MatrixD = DeltaMatrix[Double]

    override type Scalar = DualDeltaScalar[Double]
    override type ColumnVector = DualDeltaColumnVector[Double]
    override type RowVector = DualDeltaRowVector[Double]
    override type Matrix = DualDeltaMatrix[Double]


    def createScalar(value: Double, dual: ScalarD): Scalar = DualDeltaScalar(value, dual)

    def createColumnVector(value: DenseVector[Double], dual: ColumnVectorD): ColumnVector = DualDeltaColumnVector(value, dual)
 
    def createRowVector(value: Transpose[DenseVector[Double]], dual: RowVectorD): RowVector = DualDeltaRowVector(value, dual)

    def createMatrix(value: DenseMatrix[Double], dual: MatrixD): Matrix = DualDeltaMatrix(value, dual)
    
    def zeroD: ScalarD = DeltaScalar.Zero(0.0)
    
    def dTranspose(m: MatrixD): MatrixD = DeltaMatrix.Transpose(m)
    def dTransposeColumVector(v: ColumnVectorD): RowVectorD = DeltaRowVector.Transpose(v)
    def dTransposeRowVector(v: RowVectorD): ColumnVectorD = DeltaColumnVector.Transpose(v)
    def timesDMM(dm: MatrixD, m: DenseMatrix[Double]): MatrixD = DeltaMatrix.MatrixDotDMM(dm, m) 
    def timesMDM(m: DenseMatrix[Double], dm: MatrixD): MatrixD = DeltaMatrix.MatrixDotMDM(m, dm)
    def addDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = DeltaMatrix.AddDMDM(dm1, dm2)
    def timesDMCV(dm: MatrixD, v: DenseVector[Double]): ColumnVectorD = DeltaColumnVector.MatrixDot(dm, v)
    def timesMDCV(m: DenseMatrix[Double], dv: ColumnVectorD): ColumnVectorD = DeltaColumnVector.MatrixDot2(m, dv)
    def addDCVDCV(dm1: ColumnVectorD, dm2: ColumnVectorD): ColumnVectorD = DeltaColumnVector.AddVV(dm1, dm2)
    def timesDMS(dm: MatrixD, s: Double): MatrixD = ???
    def timesMDS(m: DenseMatrix[Double], ds: ScalarD): MatrixD = ???
    def timesDCVRV(dv: ColumnVectorD, v: Transpose[DenseVector[Double]]): MatrixD = DeltaMatrix.MatrixDotDCVRV(dv, v)
    def timesCVDRV(v: DenseVector[Double], dv: RowVectorD): MatrixD = DeltaMatrix.MatrixDotCVDRV(v, dv)
    def timesDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = DeltaColumnVector.AddVS(dv, DeltaScalar.Zero(s))
    def timesCVDS(v: DenseVector[Double], ds: ScalarD): ColumnVectorD = DeltaColumnVector.AddVS(DeltaColumnVector.Zero(v.length), ds)
    def addDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD = DeltaRowVector.AddVV(dv1, dv2)
    def timesDRVM(dv: RowVectorD, m: DenseMatrix[Double]): RowVectorD = ??? 
    def timesRDMV(v: Transpose[DenseVector[Double]], dv: MatrixD): RowVectorD = ???
    def addDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = DeltaScalar.Add(ds1, ds2)
    def timesDRVCV(dv: RowVectorD, v: DenseVector[Double]): ScalarD = ???
    def timesRVDCV(v: Transpose[DenseVector[Double]], dv: ColumnVectorD): ScalarD = 
        DeltaScalar.MultiplyRVDCV(v, dv)
    def timesDRVS(dv: RowVectorD, s: Double): RowVectorD = ???
    def timesRVDS(v: Transpose[DenseVector[Double]], ds: ScalarD): RowVectorD = ???
    def timesSDS(s: Double, ds: ScalarD): ScalarD = DeltaScalar.Scale(ds, s)
    def addDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD = DeltaMatrix.AddDMDCV(dm, dv)
    def addDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD = ???
    def addDMDS(dm: MatrixD, ds: ScalarD): MatrixD = ???
    def addDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = DeltaColumnVector.AddVS(dv, ds)
    def addDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD = ???
    def subDMDM(dm1: MatrixD, dm2: MatrixD): MatrixD = ???
    def subDMDCV(dm: MatrixD, dv: ColumnVectorD): MatrixD = ??? 
    def subDMDRV(dm: MatrixD, dv: RowVectorD): MatrixD = ???
    def subDMDS(dm: MatrixD, ds: ScalarD): MatrixD = ???
    def subDCVDCV(dv1: ColumnVectorD, dv2: ColumnVectorD): ColumnVectorD = DeltaColumnVector.MinusVV(dv1, dv2)
    def subDCVDS(dv: ColumnVectorD, ds: ScalarD): ColumnVectorD = ???
    def subDRVDRV(dv1: RowVectorD, dv2: RowVectorD): RowVectorD = ???
    def subDRVDS(dv: RowVectorD, ds: ScalarD): RowVectorD = ???
    def subDSDS(ds1: ScalarD, ds2: ScalarD): ScalarD = DeltaScalar.Sub(ds1, ds2)
    def divideDMS(dm: MatrixD, s: Double): MatrixD = ???
    def divideDCVS(dv: ColumnVectorD, s: Double): ColumnVectorD = ???
    def divideDRVS(dv: RowVectorD, s: Double): RowVectorD = ???
    def divideDSS(ds: ScalarD, s: Double): ScalarD = DeltaScalar.Div(ds, s)
    def sumDCV(dv: ColumnVectorD, vLength: Int): ScalarD = DeltaScalar.Sum(dv, vLength)
    def timesElementWiseMDM(m1: DenseMatrix[Double], dm2: MatrixD): MatrixD = DeltaMatrix.ElementWiseScale(m1, dm2)
    def timesElementWiseCVDCV(v1: DenseVector[Double], dv2: ColumnVectorD): ColumnVectorD = DeltaColumnVector.ElementWiseScale(v1, dv2)
    def timesElementWiseRVDRV(v1: Transpose[DenseVector[Double]], dv2: RowVectorD): RowVectorD = ???

    override def reduceCV(v: ColumnVector)(f: [T] => (T, T) => Numeric[T] ?=> T): Scalar =
        def reduceF[T: Numeric](v: Vector[T]): T = 
            v.reduce(f[T])
        import scalagrad.auto.reverse.DeriverReversePlan.given
        val dReduceF = ScalaGrad.derive(reduceF[DualDelta[Double]])
        val res = dReduceF(v.v.toScalaVector)
        DualDeltaScalar(
            v.v.reduce(f[Double]), 
            timesRVDCV(DenseVector(res.toArray).t, v.dv)
        )

/*

object BreezeVectorAlgebraForDualDeltaDouble extends LinearAlgebraOps:
    
    override type Scalar = DualScalar[Double]
    override type ColumnVector = DualColumnVector[Double]
    override type RowVector = DualRowVector[Double]
    override type Matrix = DualMatrix[Double]

    override def nRows(m: Matrix): Int = m.value.rows
    override def nCols(m: Matrix): Int = m.value.cols
    override def lengthColumnVector(v: ColumnVector): Int = v.value.length
    override def lengthRowVector(v: RowVector): Int = v.value.inner.length

    override def liftToScalar(d: Int): Scalar = DualScalar(d.toDouble, DeltaScalar.Zero(0.0))

    override def inverse(m: Matrix): Matrix = ???

    override def determinant(m: Matrix): Scalar = ???

    override def transpose(m: Matrix): Matrix = 
        DualMatrix(m.value.t, DeltaMatrix.Transpose(m.delta))

    override def transposeColumVector(v: ColumnVector): RowVector =
        DualRowVector(v.value.t, DeltaRowVector.Transpose(v.delta))

    override def transposeRowVector(v: RowVector): ColumnVector =
        DualColumnVector(v.value.t, DeltaColumnVector.Transpose(v.delta))
    
    override def timesMM(m1: Matrix, m2: Matrix): Matrix =
        def dTimes(v: DenseMatrix[Double], dv: DeltaMatrix[Double], v2: DenseMatrix[Double], dv2: DeltaMatrix[Double]): DeltaMatrix[Double] =
            DeltaMatrix.MatrixDot2(dv, v2) + DeltaMatrix.MatrixDot(v, dv2)
        DualMatrix(m1.value * m2.value, dTimes(m1.value, m1.delta, m2.value, m2.delta))
    
    override def timesMCV(m: Matrix, v: ColumnVector): ColumnVector = 
        def dTimes(v1: DenseMatrix[Double], dv1: DeltaMatrix[Double], v2: DenseVector[Double], dv2: DeltaColumnVector[Double]): DeltaColumnVector[Double] =
            DeltaColumnVector.AddVV(
                DeltaColumnVector.MatrixDot(dv1, v2), 
                DeltaColumnVector.MatrixDot2(v1, dv2)
            )
        DualColumnVector(m.value * v.value, dTimes(m.value, m.delta, v.value, v.delta))
    
    override def timesMS(m: Matrix, s: Scalar): Matrix = ???
     
    override def timesCVRV(v1: ColumnVector, v2: RowVector): Matrix =
        def dTimes(v1: DenseVector[Double], dv1: DeltaColumnVector[Double], v2: Transpose[DenseVector[Double]], dv2: DeltaRowVector[Double]): DeltaMatrix[Double] =
            DeltaMatrix.MatrixDot4(v1, dv2) + DeltaMatrix.MatrixDot3(dv1, v2)
        DualMatrix(v1.value * v2.value, dTimes(v1.value, v1.delta, v2.value, v2.delta))    

    override def timesCVS(v: ColumnVector, s: Scalar): ColumnVector = ???

    override def timesRVM(v: RowVector, m: Matrix): RowVector = ???
    override def timesRVCV(v1: RowVector, v2: ColumnVector): Scalar = ???
    override def timesRVS(v: RowVector, s: Scalar): RowVector = ???

    override def timesSS(s1: Scalar, s2: Scalar): Scalar = ???

    override def plusMM(m1: Matrix, m2: Matrix): Matrix = ???
    override def plusMCV(m: Matrix, v: ColumnVector): Matrix = ???
    override def plusMRV(m: Matrix, v: RowVector): Matrix = ???
    override def plusMS(m: Matrix, s: Scalar): Matrix = ???

    override def plusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = ???
    override def plusCVS(v: ColumnVector, s: Scalar): ColumnVector = ???

    override def plusRVRV(v1: RowVector, v2: RowVector): RowVector = ???
    override def plusRVS(v: RowVector, s: Scalar): RowVector = ???
      
    override def plusSS(s1: Scalar, s2: Scalar): Scalar = ???

    override def minusMM(m1: Matrix, m2: Matrix): Matrix = ???
    override def minusMCV(m: Matrix, v: ColumnVector): Matrix = ???
    override def minusMRV(m: Matrix, v: RowVector): Matrix = ???
    override def minusMS(m: Matrix, s: Scalar): Matrix = ???

    override def minusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = ???
    override def minusCVS(v: ColumnVector, s: Scalar): ColumnVector = ???
    
    override def minusRVRV(v1: RowVector, v2: RowVector): RowVector = ???
    override def minusRVS(v: RowVector, s: Scalar): RowVector = ???

    override def minusSS(s1: Scalar, s2: Scalar): Scalar = ???

    override def divideMS(m: Matrix, s: Scalar): Matrix = ???
    
    override def divideCVS(v: ColumnVector, s: Scalar): ColumnVector = ???
    
    override def divideRVS(v: RowVector, s: Scalar): RowVector = ???

    override def divideSS(s1: Scalar, s2: Scalar): Scalar = ???

    override def sumCV(v: ColumnVector): Scalar = ???

    override def elementWiseOpsM(v: Matrix, f: [T] => T => Numeric[T] ?=> T): Matrix = ???

    override def elementWiseOpsCV(v: ColumnVector, f: [T] => T => Numeric[T] ?=> T): ColumnVector = ???

    override def elementWiseOpsRV(v: RowVector, f: [T] => T => Numeric[T] ?=> T): RowVector = ???

    override def applyToScalar(s: Scalar, f: [T] => T => Numeric[T] ?=> T): Scalar = ???
*/