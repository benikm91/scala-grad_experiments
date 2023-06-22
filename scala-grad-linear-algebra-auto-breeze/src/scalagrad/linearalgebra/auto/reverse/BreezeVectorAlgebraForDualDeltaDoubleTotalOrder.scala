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

final case class BreezeVectorAlgebraForDualDeltaDoubleTotalOrder(private var nextIndex: Int = 0) extends LinearAlgebraOps:

    def calcNextIndex(): Int = 
        val res = nextIndex
        nextIndex += 1
        res

    type Index = Int
    val ops = BreezeVectorAlgebraForDualDeltaDouble

    override type Scalar = BreezeVectorAlgebraForDualDeltaDouble.Scalar
    override type ColumnVector = BreezeVectorAlgebraForDualDeltaDouble.ColumnVector
    override type RowVector = BreezeVectorAlgebraForDualDeltaDouble.RowVector
    override type Matrix = BreezeVectorAlgebraForDualDeltaDouble.Matrix

    def liftMatrix(m: DenseMatrix[Double]): Matrix = 
        val res = ops.liftMatrix(m)
        markIndex(res)

    def liftColumnVector(m: DenseVector[Double]): ColumnVector = 
        val res = ops.liftColumnVector(m)
        markIndex(res)

    def markIndex(m: Matrix): Matrix =
        m.dv.index = calcNextIndex()
        m

    def markIndex(cv: ColumnVector): ColumnVector =
        cv.dv.index = calcNextIndex()
        cv

    def markIndex(rv: RowVector): RowVector =
        rv.dv.index = calcNextIndex()
        rv

    def markIndex(s: Scalar): Scalar =
        s.dv.index = calcNextIndex()
        s

    override def plusMCV(m: Matrix, cv: ColumnVector): Matrix =
        val res = ops.plusMCV(m, cv)
        markIndex(res)

    override def nRows(m: Matrix): Int = ops.nRows(m)

    override def nCols(m: Matrix): Int = ops.nCols(m)

    override def lengthColumnVector(v: ColumnVector): Int = ops.lengthColumnVector(v)

    override def lengthRowVector(v: RowVector): Int = ops.lengthRowVector(v)

    override def liftToScalar(d: Int): Scalar = ops.liftToScalar(d)

    override def liftToScalar(d: Double): Scalar = ops.liftToScalar(d)

    override def scalarToDouble(d: Scalar): Double = ops.scalarToDouble(d)

    override def inverse(m: Matrix): Matrix = 
        val res = ops.inverse(m)
        markIndex(res)

    override def determinant(m: Matrix): Scalar = 
        val res = ops.determinant(m)
        markIndex(res)

    override def transpose(m: Matrix): Matrix = 
        val res = ops.transpose(m)
        markIndex(res)

    override def transposeColumVector(v: ColumnVector): RowVector = 
        val res = ops.transposeColumVector(v)
        markIndex(res)

    override def transposeRowVector(v: RowVector): ColumnVector = 
        val res = ops.transposeRowVector(v)
        markIndex(res)

    override def timesMM(m1: Matrix, m2: Matrix): Matrix = 
        val res = ops.timesMM(m1, m2)
        markIndex(res)

    override def timesMCV(m: Matrix, v: ColumnVector): ColumnVector = 
        val res = ops.timesMCV(m, v)
        markIndex(res)

    override def timesMS(m: Matrix, s: Scalar): Matrix = 
        val res = ops.timesMS(m, s)
        markIndex(res)

    override def timesCVRV(v1: ColumnVector, v2: RowVector): Matrix = 
        val res = ops.timesCVRV(v1, v2)
        markIndex(res)

    override def timesCVS(v: ColumnVector, s: Scalar): ColumnVector = 
        val res = ops.timesCVS(v, s)
        markIndex(res)

    override def timesRVM(v: RowVector, m: Matrix): RowVector = 
        val res = ops.timesRVM(v, m)
        markIndex(res)

    override def timesRVCV(v1: RowVector, v2: ColumnVector): Scalar =
        val res = ops.timesRVCV(v1, v2)
        markIndex(res)

    override def timesRVS(v: RowVector, s: Scalar): RowVector = 
        val res = ops.timesRVS(v, s)
        markIndex(res)

    override def timesSS(s1: Scalar, s2: Scalar): Scalar = 
        val res = ops.timesSS(s1, s2)
        markIndex(res)

    override def plusMM(m1: Matrix, m2: Matrix): Matrix = 
        val res = ops.plusMM(m1, m2)
        markIndex(res)

    override def plusMRV(m: Matrix, v: RowVector): Matrix = 
        val res = ops.plusMRV(m, v)
        markIndex(res)

    override def plusMS(m: Matrix, s: Scalar): Matrix = 
        val res = ops.plusMS(m, s)
        markIndex(res)

    override def plusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = 
        val res = ops.plusCVCV(v1, v2)
        markIndex(res)

    override def plusCVS(v: ColumnVector, s: Scalar): ColumnVector = 
        val res = ops.plusCVS(v, s)
        markIndex(res)

    override def plusRVRV(v1: RowVector, v2: RowVector): RowVector = 
        val res = ops.plusRVRV(v1, v2)
        markIndex(res)

    override def plusRVS(v: RowVector, s: Scalar): RowVector = 
        val res = ops.plusRVS(v, s)
        markIndex(res)

    override def plusSS(s1: Scalar, s2: Scalar): Scalar = 
        val res = ops.plusSS(s1, s2)
        markIndex(res)

    override def minusMM(m1: Matrix, m2: Matrix): Matrix = 
        val res = ops.minusMM(m1, m2)
        markIndex(res)

    override def minusMCV(m: Matrix, v: ColumnVector): Matrix = 
        val res = ops.minusMCV(m, v)
        markIndex(res)

    override def minusMRV(m: Matrix, v: RowVector): Matrix = 
        val res = ops.minusMRV(m, v)
        markIndex(res)

    override def minusMS(m: Matrix, s: Scalar): Matrix = 
        val res = ops.minusMS(m, s)
        markIndex(res)

    override def minusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = 
        val res = ops.minusCVCV(v1, v2)
        markIndex(res)

    override def minusCVS(v: ColumnVector, s: Scalar): ColumnVector = 
        val res = ops.minusCVS(v, s)
        markIndex(res)

    override def minusRVRV(v1: RowVector, v2: RowVector): RowVector = 
        val res = ops.minusRVRV(v1, v2)
        markIndex(res)

    override def minusRVS(v: RowVector, s: Scalar): RowVector = 
        val res = ops.minusRVS(v, s)
        markIndex(res)

    override def minusSS(s1: Scalar, s2: Scalar): Scalar = 
        val res = ops.minusSS(s1, s2)
        markIndex(res)

    override def divideMS(m: Matrix, s: Scalar): Matrix = 
        val res = ops.divideMS(m, s)
        markIndex(res)

    override def divideCVS(v: ColumnVector, s: Scalar): ColumnVector = 
        val res = ops.divideCVS(v, s)
        markIndex(res)

    override def divideRVS(v: RowVector, s: Scalar): RowVector = 
        val res = ops.divideRVS(v, s)
        markIndex(res)

    override def divideSS(s1: Scalar, s2: Scalar): Scalar = 
        val res = ops.divideSS(s1, s2)
        markIndex(res)

    override def foldLeftCV(s: Scalar)(v: ColumnVector)(f: (Scalar, Scalar) => Scalar): Scalar = 
        val res = ops.foldLeftCV(s)(v)(f)
        markIndex(res)

    override def sumCV(v: ColumnVector): Scalar = 
        val res = ops.sumCV(v)
        markIndex(res)

    override def sumM(v: Matrix): Scalar = 
        val res = ops.sumM(v)
        markIndex(res)

    override def elementWiseTimesMM(m1: Matrix, m2: Matrix): Matrix = 
        val res = ops.elementWiseTimesMM(m1, m2)
        markIndex(res)

    override def elementWiseTimesCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = 
        val res = ops.elementWiseTimesCVCV(v1, v2)
        markIndex(res)

    override def elementWiseOpsM(m: Matrix, f: Scalar => Scalar): Matrix = 
        val res = ops.elementWiseOpsM(m, f)
        markIndex(res)

    def elementWiseOpsMForward(m: Matrix, f: DualNumberScalar[Double] => DualNumberScalar[Double]): Matrix =
        val res = ops.elementWiseOpsMForward(m, f)
        markIndex(res)

    override def columnWiseOpsM(m: Matrix, f: ColumnVector => ColumnVector): Matrix = 
        val res = ops.columnWiseOpsM(m, f)
        markIndex(res)

    override def rowWiseOpsM(m: Matrix, f: RowVector => RowVector): Matrix = 
        val res = ops.rowWiseOpsM(m, f)
        markIndex(res)

    def rowWiseOpsMForward(m: Matrix, op: DualNumberRowVector[Double] => DualNumberRowVector[Double]): Matrix =
        val res = ops.rowWiseOpsMForward(m, op)
        markIndex(res)
    
    override def elementWiseOpsCV(v: ColumnVector, f: Scalar => Scalar): ColumnVector = 
        val res = ops.elementWiseOpsCV(v, f)
        markIndex(res)

    override def elementWiseOpsRV(v: RowVector, f: Scalar => Scalar): RowVector = 
        val res = ops.elementWiseOpsRV(v, f)
        markIndex(res)

    override def elementAtM(m: Matrix, rowI: Int, columnJ: Int): Scalar = 
        val res = ops.elementAtM(m, rowI, columnJ)
        markIndex(res)

    override def rowAtM(m: Matrix, rowI: Int): RowVector = 
        val res = ops.rowAtM(m, rowI)
        markIndex(res)

    override def fromElements(nRows: Int, nCols: Int, elements: Scalar*): Matrix =
        val res = ops.fromElements(nRows, nCols, elements: _*)
        markIndex(res)

    override def stackRows(rows: RowVector*): Matrix = 
        val res = ops.stackRows(rows: _*)
        markIndex(res)

    override def elementAtCV(v: ColumnVector, i: Int): Scalar = 
        val res = ops.elementAtCV(v, i)
        markIndex(res)
