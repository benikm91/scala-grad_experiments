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

object BreezeVectorAlgebraForDouble extends LinearAlgebraOps:
    
    override type Scalar = Double
    override type ColumnVector = DenseVector[Double]
    override type RowVector = Transpose[DenseVector[Double]]
    override type Matrix = DenseMatrix[Double]

    override def nRows(m: Matrix): Int = m.rows
    override def nCols(m: Matrix): Int = m.cols
    override def lengthColumnVector(v: ColumnVector): Int = v.length
    override def lengthRowVector(v: RowVector): Int = v.inner.length

    override def liftToScalar(d: Int): Scalar = d.toDouble
    override def liftToScalar(d: Double): Scalar = d

    override def inverse(m: Matrix): Matrix = ???

    override def determinant(m: Matrix): Scalar = ???

    override def transpose(m: Matrix): Matrix = m.t

    override def transposeColumVector(v: ColumnVector): RowVector = v.t

    override def transposeRowVector(v: RowVector): ColumnVector = v.t
    
    override def timesMM(m1: Matrix, m2: Matrix): Matrix = m1 * m2
    override def timesMCV(m: Matrix, v: ColumnVector): ColumnVector = m * v
    override def timesMS(m: Matrix, s: Scalar): Matrix = m * s
    
    override def timesCVRV(v1: ColumnVector, v2: RowVector): Matrix = v1 * v2
    override def timesCVS(v: ColumnVector, s: Scalar): ColumnVector = v * s

    override def timesRVM(v: RowVector, m: Matrix): RowVector = v * m
    override def timesRVCV(v1: RowVector, v2: ColumnVector): Scalar = v1 * v2
    override def timesRVS(v: RowVector, s: Scalar): RowVector = v * s

    override def timesSM(s: Scalar, m: Matrix): Matrix = timesMS(m, s)
    override def timesSCV(s: Scalar, v: ColumnVector): ColumnVector = timesCVS(v, s)
    override def timesSRV(s: Scalar, v: RowVector): RowVector = timesRVS(v, s)
    override def timesSS(s1: Scalar, s2: Scalar): Scalar = s1 * s2

    override def plusMM(m1: Matrix, m2: Matrix): Matrix = m1 + m2
    override def plusMCV(m: Matrix, v: ColumnVector): Matrix = m(breeze.linalg.*, ::) + v
    override def plusMRV(m: Matrix, v: RowVector): Matrix = m(::, breeze.linalg.*) + v.t
    override def plusMS(m: Matrix, s: Scalar): Matrix = m + s

    override def plusCVM(v: ColumnVector, m: Matrix): Matrix = plusMCV(m, v)
    override def plusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = v1 + v2
    override def plusCVS(v: ColumnVector, s: Scalar): ColumnVector = v + s

    override def plusRVM(v: RowVector, m: Matrix): Matrix = plusMRV(m, v)
    override def plusRVRV(v1: RowVector, v2: RowVector): RowVector = v1 + v2
    override def plusRVS(v: RowVector, s: Scalar): RowVector = v + s
        
    override def plusSM(s: Scalar, m: Matrix): Matrix = plusMS(m, s)
    override def plusSCV(s: Scalar, v: ColumnVector): ColumnVector = plusCVS(v, s)
    override def plusSRV(s: Scalar, v: RowVector): RowVector = plusRVS(v, s)
    override def plusSS(s1: Scalar, s2: Scalar): Scalar = s1 + s2

    override def minusMM(m1: Matrix, m2: Matrix): Matrix = m1 - m2
    override def minusMCV(m: Matrix, v: ColumnVector): Matrix = m(breeze.linalg.*, ::) - v
    override def minusMRV(m: Matrix, v: RowVector): Matrix = m(::, breeze.linalg.*) - v.t
    override def minusMS(m: Matrix, s: Scalar): Matrix = m - s

    override def minusCVM(v: ColumnVector, m: Matrix): Matrix = minusMCV(m, v)
    override def minusCVCV(v1: ColumnVector, v2: ColumnVector): ColumnVector = v1 - v2
    override def minusCVS(v: ColumnVector, s: Scalar): ColumnVector = v - s
    
    override def minusRVM(v: RowVector, m: Matrix): Matrix = minusMRV(m, v)
    override def minusRVRV(v1: RowVector, v2: RowVector): RowVector = v1 - v2
    override def minusRVS(v: RowVector, s: Scalar): RowVector = v - s

    override def minusSM(s: Scalar, m: Matrix): Matrix = minusMS(m, s)
    override def minusSCV(s: Scalar, v: ColumnVector): ColumnVector = minusCVS(v, s)
    override def minusSRV(s: Scalar, v: RowVector): RowVector = minusRVS(v, s)
    override def minusSS(s1: Scalar, s2: Scalar): Scalar = s1 - s2

    override def divideMS(m: Matrix, s: Scalar): Matrix = m / s
    
    override def divideCVS(v: ColumnVector, s: Scalar): ColumnVector = v / s
    
    override def divideRVS(v: RowVector, s: Scalar): RowVector = v / s

    override def divideSM(s: Scalar, m: Matrix): Matrix = divideMS(m, s)
    override def divideSCV(s: Scalar, v: ColumnVector): ColumnVector = divideCVS(v, s)
    override def divideSRV(s: Scalar, v: RowVector): RowVector = divideRVS(v, s)
    override def divideSS(s1: Scalar, s2: Scalar): Scalar = s1 / s2

    override def reduceCV(v: ColumnVector)(f: [T] => (T, T) => Numeric[T] ?=> T): Scalar =
        v.reduce(f[Double])    

    override def sumCV(v: ColumnVector): Scalar = breeze.linalg.sum(v)

    override def elementWiseOpsM(v: Matrix, f: Scalar => Scalar): Matrix = 
        v.map(f)

    override def elementWiseOpsCV(v: ColumnVector, f: Scalar => Scalar): ColumnVector =
        v.map(f)

    override def elementWiseOpsRV(v: RowVector, f: Scalar => Scalar): RowVector =
        v.t.map(f).t

    override def elementAtCV(v: DenseVector[Double], i: Int): Scalar = v(i)

    override def elementAtM(m: DenseMatrix[Double], i: Int, j: Int): Scalar = m(i, j)
