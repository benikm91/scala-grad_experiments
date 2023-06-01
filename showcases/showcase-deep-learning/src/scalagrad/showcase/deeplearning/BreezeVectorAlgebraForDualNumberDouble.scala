package scalagrad.showcase.deeplearning

import scalagrad.api.ScalaGrad
import scalagrad.api.Dual
import scalagrad.api.VectorAlgebraFor
import scalagrad.api.VectorAlgebraOps
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

object BreezeVectorAlgebraForDualNumberDouble extends VectorAlgebraOps:
    
    override type Scalar = DualNumber[Double]
    override type ColumnVector = (DenseVector[Double], DenseVector[Double])
    override type RowVector = (Transpose[DenseVector[Double]], Transpose[DenseVector[Double]])
    override type Matrix = (DenseMatrix[Double], DenseMatrix[Double])

    def liftToScalar(d: Int): Scalar = DualNumber(d.toDouble, 0.0)

    override def transpose(m: Matrix): Matrix = (m._1.t, m._2.t)
    override def transposeColumVector(v: ColumnVector): RowVector = (v._1.t, v._2.t)
    override def transposeRowVector(v: RowVector): ColumnVector = (v._1.t, v._2.t)

    override def timesMM(m1: Matrix, m2: Matrix): Matrix =
        // derivative of dot product
        def dTimes(m: DenseMatrix[Double], dm: DenseMatrix[Double], m2: DenseMatrix[Double], dm2: DenseMatrix[Double]): DenseMatrix[Double] =
            dm * m2 + m * dm2
        (m1._1 * m2._1, dTimes(m1._1, m1._2, m2._1, m2._2))
    override def timesVV(v1: RowVector, v2: ColumnVector): Scalar = 
        def dTimes(v1: Transpose[DenseVector[Double]], dv1: Transpose[DenseVector[Double]], v2: DenseVector[Double], dv2: DenseVector[Double]): Double =
            dv1 * v2 + v1 * dv2
        DualNumber(v1._1 * v2._1, dTimes(v1._1, v1._2, v2._1, v2._2))
    override def timesOuterVV(v1: ColumnVector, v2: RowVector): Matrix = 
        def dTimes(v1: DenseVector[Double], dv1: DenseVector[Double], v2: Transpose[DenseVector[Double]], dv2: Transpose[DenseVector[Double]]): DenseMatrix[Double] =
            dv1 * v2 + v1 * dv2
        (v1._1 * v2._1, dTimes(v1._1, v1._2, v2._1, v2._2))
    override def timesVM(v: RowVector, m: Matrix): RowVector = 
        def dTimes(v1: Transpose[DenseVector[Double]], dv1: Transpose[DenseVector[Double]], v2: DenseMatrix[Double], dv2: DenseMatrix[Double]): Transpose[DenseVector[Double]] =
            dv1 * v2.t + v1 * dv2.t
        (v._1 * m._1, dTimes(v._1, v._2, m._1, m._2))
    override def timesMV(m: Matrix, v: ColumnVector): ColumnVector =
        def dTimes(v1: DenseMatrix[Double], dv1: DenseMatrix[Double], v2: DenseVector[Double], dv2: DenseVector[Double]): DenseVector[Double] =
            dv1 * v2 + v1 * dv2
        (m._1 * v._1, dTimes(m._1, m._2, v._1, v._2))
    override def timesVS(v: ColumnVector, s: Scalar): ColumnVector =
        (v._1 * s.value, v._2 * s.derivative)
    override def timesMS(m: Matrix, s: Scalar): Matrix = 
        (m._1 * s.value, m._2 * s.derivative)

    override def timesSS(s1: Scalar, s2: Scalar): Scalar =
        def dTimes(s1: Double, ds1: Double, s2: Double, ds2: Double): Double =
            ds1 * s2 + s1 * ds2
        DualNumber(s1.value * s2.value, dTimes(s1.value, s1.derivative, s2.value, s2.derivative))

    override def plusMM(m1: Matrix, m2: Matrix): Matrix =
        (m1._1 + m2._1, m1._2 + m2._2)
    override def plusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector =
        (v1._1 + v2._1, v1._2 + v2._2)
    override def plusMV(m: Matrix, v: ColumnVector): Matrix =
        (m._1(breeze.linalg.*, ::) + v._1, m._2(breeze.linalg.*, ::) + v._2)
    override def plusVS(v: ColumnVector, s: Scalar): ColumnVector =
        (v._1 + s.value, v._2 + s.derivative)
    override def plusMS(m: Matrix, s: Scalar): Matrix = 
        (m._1 + s.value, m._2 + s.derivative)
    override def plusSS(s1: Scalar, s2: Scalar): Scalar =
        DualNumber(s1.value + s2.value, s1.derivative + s2.derivative)
    override def minusMM(m1: Matrix, m2: Matrix): Matrix =
        (m1._1 - m2._1, m1._2 - m2._2)
    override def minusVV(v1: ColumnVector, v2: ColumnVector): ColumnVector =
        (v1._1 - v2._1, v1._2 - v2._2)
    override def minusMV(m: Matrix, v: ColumnVector): Matrix = ???
    override def minusVM(v: ColumnVector, m: Matrix): Matrix = ???
    override def minusVS(v: ColumnVector, s: Scalar): ColumnVector =
        (v._1 - s.value, v._2 - s.derivative)
    override def minusMS(m: Matrix, s: Scalar): Matrix =
        (m._1 - s.value, m._2 - s.derivative)
    override def minusSS(s1: Scalar, s2: Scalar): Scalar =
        DualNumber(s1.value - s2.value, s1.derivative - s2.derivative)

    override def elementWiseOps(v: ColumnVector, f: [T] => T => Numeric[T] ?=> T): ColumnVector = 
        import scalagrad.auto.forward.DeriverForwardPlan.given
        val df = ScalaGrad.derive(f[DualNumber[Double]])
        (v._1.map(f[Double]), v._1.map(df) *:* v._2)

    override def elementWiseOpsM(v: Matrix, f: [T] => (x: T) => (Numeric[T]) ?=> T): Matrix =
        import scalagrad.auto.forward.DeriverForwardPlan.given
        val df = ScalaGrad.derive(f[DualNumber[Double]])
        (v._1.map(f[Double]), v._1.map(df) *:* v._2)

    override def length(v: ColumnVector): Int = v._1.length

    override def divideSS(s1: Scalar, s2: Scalar): Scalar = 
        def dDivide(u: Double, du: Double, v: Double, dv: Double): Double =
            (du * v - u * dv) / (v * v)
        DualNumber(s1.value / s2.value, dDivide(s1._1, s1._2, s2._1, s2._2))

    override def sum(v: ColumnVector): Scalar =
        DualNumber(breeze.linalg.sum(v._1), breeze.linalg.sum(v._2))
